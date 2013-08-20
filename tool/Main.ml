let expected_version = "4.00.1"

exception File_not_found of string

let assert_file_exists (filename : string) : unit =
  if not (Sys.file_exists filename) then
  	raise (File_not_found filename)

(* Runs an executable and waits for termination, returning the exit code.
	 If the process is killed or stopped, prints a warning returns -1. *)
let run_process (filename : string) (args : string list) : int =
	let open Unix in
	(* TODO(arjun): printed string should be shell-escaped so you can copy it
	   to the terminal. *)
  print_string (String.concat " " (filename :: args));
	print_newline (); 
	let pid = create_process filename (Array.of_list (filename :: args))
	  stdin stdout stderr in
	let (_, exit_status) = Unix.waitpid [] pid in
	match exit_status with
	  | WEXITED exit_code -> exit_code
	  | WSIGNALED n ->
	  	  Format.printf "Sub-process killed (signal %d)\n%!" n;
	  	  -1
	  | WSTOPPED n ->
	  	  Format.printf "Sub-process stopped (signal %d)\n%!" n;
	  	  -1

let check_code (return_code : int) : unit =
	(if return_code <> 0 then
	  Format.printf "Error (exit code %d)\n%!" return_code);
	exit return_code

(* Use ocamlbuild to clean. *)
let clean () : unit =
  check_code (Sys.command "ocamlbuild -classic-display -clean")

(* Uses the oUnit and cs691f packages and the oUnit syntax extension. *)
let build (main_module : string) : unit =
	assert_file_exists (main_module ^ ".ml");
	let target = Format.sprintf "%s.d.byte" main_module in
  check_code (run_process "ocamlbuild" [
  	"-use-ocamlfind"; "-classic-display"; "-no-links";
	   "-tag-line"; "<*.ml{,i}> : syntax(camlp4o), \
	   	                          package(pa_ounit.syntax), \
	                              package(oUnit), \
	                              package(cs691f)";
	   "-tag-line"; "<*.d.byte> : package(pa_ounit), \
	                              package(oUnit), \
	                              package(cs691f)";
	   "-tag-line"; "<*.native> : package(pa_ounit), \
	                              package(oUnit), \
	                              package(cs691f)";
	   target
	])

(* Runs unit tests using the oUnit syntax extension's test runner. *)
let test (main_module : string) : unit =
  let cmd = Format.sprintf "_build/%s.d.byte"	main_module in
	assert_file_exists cmd;
	check_code (Sys.command (Format.sprintf
		"%s inline-test-runner dummy" cmd))

let run (main_module : string) (args : string list) =
  let cmd = Format.sprintf "_build/%s.d.byte"	main_module in
	assert_file_exists cmd;
	check_code (run_process cmd args)

let help () =
	let pr s = print_string s; print_newline () in
	pr "Usage: cs691f COMMMAND [args]";
	pr "";
	pr "  cs691f compile File    Compile File.ml";
	pr "  cs691f run File        Run the program File.ml.";
	pr "  cs691f test File       Run the tests in Module.ml.";
	pr "  cs691f clean           Removes files created by 'cs691f compile'.";
	pr "  cs691f help            Displays this message."

(* Use OCAMLRUNPARAM to enable stack traces, unless you've set your own. *)
let config_env () =
	try
		let _ = Unix.getenv "OCAMLRUNPARAM" in
		()
	with
	| Not_found -> Unix.putenv "OCAMLRUNPARAM" "b"

let () = 
  config_env ();
  try
	  match Array.to_list Sys.argv with
	  | [ _; "help" ]  -> help ()
	  | [ _; "clean" ] -> clean ()
	  | [ _; "compile"; target ] -> build target
	  | [ _; "test"; target ] -> test target
	  | _ :: "run" :: target :: args -> run target args
	  | _ -> print_string "Invalid arguments.\n"; help ()
	with File_not_found filename ->
		Format.printf "Could not find the file %s.\n%!" filename
