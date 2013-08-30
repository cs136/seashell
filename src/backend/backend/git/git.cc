/**
 * Seashell's git repository backend.
 * Copyright (C) 2013 The Seashell Maintainers.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>. 
 */
#include <git2.h>
#include <sys/types.h>
#include <pwd.h>
#include <string>
#include <vector>
#include <string.h>

using std::vector;
using std::string;

/** Structure representing a git repository update. */
struct seashell_git_update {
  vector<string> files;
  vector<string> files_to_delete;
  string target;
};

/**
 * seashell_git_error (void)
 * Returns the last libgit2 error string.
 *
 * Returns:
 *  See above.
 */
extern "C" const char* seashell_git_error (void) {
  if (giterr_last()) {
    return giterr_last()->message;
  }
  return NULL;
}

/**
 * seashell_git_clone (const char* repository, const char* target) 
 * Clones repository into target.
 *
 * Arguments:
 *  repository - Source repository.
 *  target - Target directory.
 * 
 * Returns:
 *  0 on success, nonzero otherwise.  Consult seashell_git_error.
 */
extern "C" int seashell_git_clone (const char* repository, const char* target) {
  git_repository* repo = NULL;
  int ret = git_clone(&repo, repository, target, NULL);
  git_repository_free(repo);

  return ret;
}

/**
 * seashell_git_init (const char* target)
 * Creates a blank git repository at target.
 *
 * Arguments:
 *  target - Target directory.
 *
 * Returns:
 *  0 on success, nonzero otherwise.  Consult seashell_git_error.
 */
extern "C" int seashell_git_init (const char* target) {
  git_repository* repo = NULL;
  int ret = git_repository_init(&repo, target, false);
  git_repository_free(repo);

  return ret;
}

/**
 * seashell_git_commit_free (struct seashell_git_update* update)
 * Deletes a Seashell git update request.
 *
 * Arguments:
 *  update - Seashell git update request.
 */
extern "C" void seashell_git_commit_free (struct seashell_git_update* update) {
  delete update;
}

/**
 * seashell_git_commit_init (const char* target)
 * Creates a new commit request object for the repository located
 * at target.
 *
 * Arguments:
 *  target - Target directory.
 *
 * Returns:
 *  New seashell_git_update object.
 */
extern "C" struct seashell_git_update* seashell_git_commit_init (const char* target) {
  seashell_git_update* result = new seashell_git_update;

  if (result) {
    result->target = target;
  }

  return result;
}

/**
 * seashell_git_commit_add (struct seashell_git_update* update, const char* file)
 * Adds a file to the git update target. 
 *
 * Arguments:
 *  update - Seashell git commit update.
 *  file - File to add.  NOTE:  Path MUST be relative to repository.
 */
extern "C" void seashell_git_commit_add (struct seashell_git_update* update, const char* file) {
  update->files.push_back(file);
}

/**
 * seashell_git_commit_delete (struct seashell_git_update* update, const char* file)
 * Adds a file to be deleted to the git update target.
 *
 * Argument:
 *  update - Seashell git commit update.
 *  file - File to delete.  NOTE:  Path MUST be relative to repository.
 */
extern "C" void seashell_git_commit_delete (struct seashell_git_update* update, const char* file) {
  update->files_to_delete.push_back(file);
}

/**
 * seashell_git_commit (struct seashell_git_update* update)
 * Runs the commit.
 *
 * Arguments:
 *  update - Update to commit.
 *
 * Returns:
 *  0 on success, nonzero otherwise.  Consult seashell_git_error.
 */
extern "C" int seashell_git_commit (struct seashell_git_update* update) {
  int ret = 0;
  git_repository* repo = NULL;
  git_index* index = NULL;
  git_oid oid;
  struct passwd* passwd = getpwuid(getuid());
  struct git_signature* authour = NULL;
  struct git_signature* committer = NULL;
  git_commit* parent = NULL;
  git_tree* tree = NULL;
  char *gecos = NULL, *user = NULL;

  if (!passwd) 
   return 1; 

  /** Parse the darned gecos field. */
  gecos = strdup(passwd->pw_gecos);
  if (!gecos)
    goto end;

  user = strtok(gecos, ",");
  
  /** Set up the commit signature. */ 
  ret = git_signature_new(&authour, user ? user : passwd->pw_name,
     passwd->pw_name, time(NULL), 0);
  if (ret)
    goto end;
  ret = git_signature_new(&committer, "Seashell",
     "seashell@cs.uwaterloo.ca", time(NULL), 0);
  if (ret)
    goto end;

  /** Open the repository. */
  ret = git_repository_open(&repo, update->target.c_str());
  if (ret) 
    goto end;

  /** Query for the last commit. */
  ret = git_reference_name_to_id(&oid, repo, "HEAD");
  if (ret)
    goto end;
  ret = git_commit_lookup(&parent, repo, &oid);
  if (ret)
    goto end;

  /** Create new commit. */
  ret = git_repository_index(&index, repo);
  if (ret)
    goto end;
  /** Add files to update. */
  for (string file : update->files) {
    ret = git_index_add_bypath(index, file.c_str());
    if (ret)
      goto end;
  }
  /** Remove files that need to be deleted. */
  for (string file : update->files_to_delete) {
    ret = git_index_remove_bypath(index, file.c_str());
    if (ret)
      goto end;
  }
  /** Write the new index. */
  ret = git_index_write(index);
  if (ret)
    goto end;
  ret = git_index_write_tree(&oid, index);
  if (ret)
    goto end;

  /** Fetch the tree we wrote. */
  ret = git_tree_lookup(&tree, repo, &oid);
  if (ret)
    goto end;

  /** Write the commit. */
  ret = git_commit_create_v(
      &oid,
      repo,
      "HEAD",
      authour,
      committer,
      NULL,
      "Automatically generated Seashell commit.",
      tree,
      1,
      parent);
  
  if (ret) 
    goto end;

end:
  git_tree_free(tree);
  git_index_free(index);
  git_commit_free(parent);
  git_repository_free(repo);
  git_signature_free(authour);
  git_signature_free(committer);
  free(gecos);

  return ret;
}
