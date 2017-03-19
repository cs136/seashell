#lang racket

(require racket/date
         db
         db/util/datetime
         json
         (file "/path/to/marmoset/db/file.rkt")
         (file "/path/to/marmoset/coursepk.rkt"))

(define stderr (current-error-port))

(define (print-help)
  (fprintf stderr "This program expects three command line arguments:\n")
  (fprintf stderr "Argument 1: Quest ID of student whose test results you want to look up.\n")
  (fprintf stderr "Argument 2: The type of test that you want results for. Can be public or secret.\n")
  (fprintf stderr "Argument 3: The project name. Get this from Project column in Marmoset course home page.\n"))

;; Check command line arguments.
(define cmd-line-args (current-command-line-arguments))
(when (not (and (equal? (vector-length cmd-line-args) 3)
                (member (vector-ref cmd-line-args 1) '("public" "secret"))))
  (print-help)
  (exit 1))

(define username (vector-ref cmd-line-args 0))
(define test-type (vector-ref cmd-line-args 1))
(define project-name (vector-ref cmd-line-args 2))

;; Debuging
;(fprintf stderr "Username: ~a\n" username)
;(fprintf stderr "Test type: ~a\n" test-type)
;(fprintf stderr "Project name: ~a\n" project-name)
;(fprintf stderr "Course pk: ~a\n" coursepk)

(define (report-error-and-quit err)
  (write-json `#hash((error . #t) (result . ,err)))
  (exit 0))

(define (list<=? lst1 lst2)
  (or (empty? lst1)
      (< (first lst1) (first lst2))
      (and (= (first lst1) (first lst2))
           (list<=? (rest lst1) (rest lst2)))))

(define (sql-timestamp->string ts)
  (format "~a-~a-~a ~a:~a:~a"
          (sql-timestamp-year ts) (sql-timestamp-month ts) (sql-timestamp-day ts)
          (sql-timestamp-hour ts) (sql-timestamp-minute ts) (sql-timestamp-second ts)))

;; Give user secret test results only if the due date ("late" time in Marmoset) has passed.
(when (equal? test-type "secret")
  (define due-date-query "SELECT ontime, late FROM projects WHERE course_pk = ? AND project_number = ?")
  (define result (query-rows marmoset-db due-date-query coursepk project-name))
  (cond [(empty? result) (report-error-and-quit (format "Could not find project ~a." project-name))]
        [(cons? (rest result))
         (report-error-and-quit (format "Multiple projects with name ~a exist in Marmoset." project-name))]
        [else
         (match-define (vector ontime late) (first result))
         (define now (current-date))
         (define past-duedate? (list<=? (list (sql-timestamp-year late)   (sql-timestamp-month late)
                                              (sql-timestamp-day late)    (sql-timestamp-hour late)
                                              (sql-timestamp-minute late) (sql-timestamp-second late))
                                        (list (date-year now) (date-month now) (date-day now)
                                              (date-hour now) (date-minute now) (date-second now))))
         ;; Debugging
         ;(fprintf stderr "Now: ~a, Due date: ~a, Past due date? ~a\n" now late past-duedate?)
         (when (not past-duedate?)
           (report-error-and-quit (format "Project ~a's due date ~a has not passed yet."
                                          project-name (sql-timestamp->string late))))]))

(define marmoset-query "SELECT stu_r.student_registration_pk, proj.project_number, sub.submission_pk,
                tout.test_type, tout.point_value, sub.submission_timestamp, sub.build_status,
                tout.outcome, tout.test_name, tout.short_test_result, tout.long_test_result,
    sub.num_public_tests_passed, test.num_public_tests
        FROM students AS stu 
        RIGHT JOIN student_registration AS stu_r ON stu.student_pk = stu_r.student_pk
        RIGHT JOIN submissions AS sub ON stu_r.student_registration_pk = sub.student_registration_pk
        LEFT JOIN projects AS proj ON sub.project_pk = proj.project_pk
        RIGHT JOIN project_jarfiles AS test ON sub.project_pk = test.project_pk
        LEFT JOIN test_runs AS tr ON sub.current_test_run_pk = tr.test_run_pk
        LEFT JOIN test_outcomes AS tout ON sub.current_test_run_pk = tout.test_run_pk AND tout.test_type = ?
        WHERE stu_r.course_pk = ? AND stu.campus_uid = ?
  AND test.jarfile_status = 'active'
        AND proj.project_number LIKE ? ORDER BY sub.submission_timestamp DESC")

(define result (query-rows marmoset-db marmoset-query test-type coursepk username project-name))

(define (row->json row)
  (match-define
    (vector student-reg project-number submission
            test-type points _timestamp build-status outcome
            test-name short-result long-result
            old-passed old-total)
    (vector-map sql-null->false row))
  `#hash((student . ,student-reg)
         (project . ,project-number)
         (submission . ,submission)
         (type . ,test-type)
         (points . ,points)
         (passed . ,old-passed)
         (all . ,old-total)
         (timestamp . 
                    ,(parameterize
                         ([date-display-format 'iso-8601])
                       (date->string (sql-datetime->srfi-date _timestamp) #t)))
         (status . ,build-status)
         (outcome . ,outcome)
         (name . ,test-name)
         (short . ,short-result)
         (long . ,long-result)))

(write-json
 `#hash((error . #f)
        (result . ,(map row->json result))))
