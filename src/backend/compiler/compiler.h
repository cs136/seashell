/**
 * Seashell's LLVM and Clang interface.
 * Copyright (C) 2013-2015 The Seashell Maintainers.
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

#ifndef __COMPILER_H__
#define __COMPILER_H__
struct seashell_compiler;
struct seashell_preprocessor;

#ifdef __EMSCRIPTEN__
#include <string>
std::string seashell_clang_version();
void seashell_compiler_add_file (struct seashell_compiler* compiler, std::string file);
void seashell_compiler_add_compile_flag (struct seashell_compiler* compiler, std::string flag);
std::string seashell_compiler_get_object(struct seashell_compiler* compiler);
std::string seashell_compiler_get_linker_messages(struct seashell_compiler* compiler);
std::string seashell_compiler_get_diagnostic_file(struct seashell_compiler* compiler, int n, int k);
std::string seashell_compiler_get_diagnostic_message(struct seashell_compiler* compiler, int n, int k);
std::string seashell_compiler_object_arch(struct seashell_compiler* compiler);
std::string seashell_compiler_object_os (struct seashell_compiler* compiler);

void seashell_preprocessor_set_main_file(struct seashell_preprocessor *preprocessor, std::string file);
std::string seashell_preprocessor_get_main_file(struct seashell_preprocessor *preprocessor);
std::string seashell_preprocessor_get_include(struct seashell_preprocessor *preprocessor, int n);
#else
extern "C" const char* seashell_clang_version();
extern "C" void seashell_compiler_add_file (struct seashell_compiler* compiler, const char* file);
extern "C" void seashell_compiler_add_compile_flag (struct seashell_compiler* compiler, const char* flag);
extern "C" const char * seashell_compiler_get_object (struct seashell_compiler* compiler, int * length);
extern "C" const char * seashell_compiler_get_linker_messages(struct seashell_compiler* compiler);
extern "C" const char * seashell_compiler_get_diagnostic_file (struct seashell_compiler* compiler, int n, int k);
extern "C" const char * seashell_compiler_get_diagnostic_message (struct seashell_compiler* compiler, int n, int k);
extern "C" const char* seashell_compiler_object_arch (struct seashell_compiler* compiler);
extern "C" const char* seashell_compiler_object_os (struct seashell_compiler* compiler);

extern "C" void seashell_preprocessor_set_main_file(struct seashell_preprocessor *preprocessor, const char *file);
extern "C" const char *seashell_preprocessor_get_main_file(struct seashell_preprocessor *preprocessor);
extern "C" const char *seashell_preprocessor_get_include(struct seashell_preprocessor *preprocessor, int n);
#endif

extern "C" struct seashell_compiler* seashell_compiler_make (void);
extern "C" void seashell_compiler_free (struct seashell_compiler* compiler);
extern "C" void seashell_compiler_clear_files (struct seashell_compiler* compiler);
extern "C" void seashell_compiler_clear_compile_flags (struct seashell_compiler* compiler);
extern "C" int seashell_compiler_get_diagnostic_count (struct seashell_compiler* compiler, int n);
extern "C" int seashell_compiler_get_diagnostic_line (struct seashell_compiler* compiler, int n, int k);
extern "C" int seashell_compiler_get_diagnostic_column (struct seashell_compiler* compiler, int n, int k);
extern "C" bool seashell_compiler_get_diagnostic_error (struct seashell_compiler* compiler, int n, int k);
extern "C" int seashell_compiler_run (struct seashell_compiler* compiler);

extern "C" struct seashell_preprocessor *seashell_preprocessor_make(void);
extern "C" void seashell_preprocessor_free(struct seashell_preprocessor *preprocessor);
extern "C" int seashell_preprocessor_get_include_count(struct seashell_preprocessor *preprocessor);
extern "C" int seashell_preprocessor_run(struct seashell_preprocessor *preprocessor);
#endif
