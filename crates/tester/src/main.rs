use std::{fs::File, path::PathBuf, process::Command};

use color_eyre::eyre::{eyre, Context, Result};
use structopt::StructOpt;
use walkdir::{DirEntry, WalkDir};

#[derive(Debug, StructOpt)]
struct Args {
    /// Path to the Lox implementation to execute.
    implementation: PathBuf,

    /// Path to the directory containing the tests.
    tests_root: PathBuf,

    /// Path to where the stdout and stderr outputs are stored.
    output_root: PathBuf,

    /// Generate the outputs rather than read.
    #[structopt(short)]
    generate: bool,
}

fn get_tests(args: &Args) -> Result<Vec<DirEntry>> {
    let mut tests = Vec::new();

    for entry in WalkDir::new(&args.tests_root) {
        let entry = entry?;
        if entry.metadata()?.is_dir() {
            continue;
        }

        if entry
            .path()
            .parent()
            .map(|p| p.ends_with("benchmark"))
            .unwrap_or(true)
        {
            continue;
        }

        if matches!(entry.path().extension(), Some(ext) if ext == "lox") {
            tests.push(entry);
        }
    }

    Ok(tests)
}

fn generate_outputs(args: &Args, tests: &[DirEntry], name_len: usize) -> Result<()> {
    for test in tests {
        let relative_path = test.path().strip_prefix(&args.tests_root)?;

        print!(
            "generating {:<padding$} ...",
            relative_path.display(),
            padding = name_len + 2
        );

        let relative_dir = test
            .path()
            .parent()
            .unwrap()
            .strip_prefix(&args.tests_root)?;
        let filename = test.path().file_stem().unwrap();

        let output_dir = args.output_root.join(relative_dir);
        std::fs::create_dir_all(&output_dir)
            .with_context(|| eyre!("failed to create output path `{}`", output_dir.display()))?;

        let mut stdout_path = output_dir.join(filename);
        stdout_path.set_extension("stdout");

        let mut stderr_path = stdout_path.clone();
        stderr_path.set_extension("stderr");

        let stdout = match File::create(&stdout_path) {
            Ok(file) => file,
            Err(e) => {
                println!("{}", e);
                continue;
            }
        };
        let stderr = match File::create(&stderr_path) {
            Ok(file) => file,
            Err(e) => {
                println!("{}", e);
                continue;
            }
        };

        let cmd = Command::new(&args.implementation)
            .arg(test.path())
            .stderr(stderr)
            .stdout(stdout)
            .status();

        match cmd {
            Ok(_) => println!("done"),
            Err(e) => print!("{}", e),
        }
    }

    Ok(())
}

fn run_tests(args: &Args, tests: &[DirEntry], name_len: usize) -> Result<()> {
    let mut succeeded = 0;
    for test in tests {
        let relative_path = test.path().strip_prefix(&args.tests_root)?;

        print!(
            "testing {:<padding$} ...",
            relative_path.display(),
            padding = name_len + 2
        );

        let relative_dir = test
            .path()
            .parent()
            .unwrap()
            .strip_prefix(&args.tests_root)?;
        let filename = test.path().file_stem().unwrap();

        let output_dir = args.output_root.join(relative_dir);
        std::fs::create_dir_all(&output_dir)
            .with_context(|| eyre!("failed to create output path `{}`", output_dir.display()))?;

        let mut stdout_path = output_dir.join(filename);
        stdout_path.set_extension("stdout");

        let mut stderr_path = stdout_path.clone();
        stderr_path.set_extension("stderr");

        let stdout_file = match std::fs::read(&stdout_path) {
            Ok(file) => file,
            Err(e) => {
                println!("{}", e);
                continue;
            }
        };
        let stderr_file = match std::fs::read(&stderr_path) {
            Ok(file) => file,
            Err(e) => {
                println!("{}", e);
                continue;
            }
        };

        let cmd = Command::new(&args.implementation).arg(test.path()).output();

        let output = match cmd {
            Ok(output) => output,
            Err(e) => {
                print!("{}", e);
                continue;
            }
        };

        print!("stderr: ");
        if output.stderr == stderr_file {
            print!("Ok    ")
        } else {
            print!("failed");
        }

        print!(" stdout: ");
        if output.stdout == stdout_file {
            println!("Ok")
        } else {
            println!("failed");
        }

        succeeded += (output.stderr == stderr_file && output.stdout == stdout_file) as usize;
    }

    println!(
        "\n{} tests, {} succeeded {} failed",
        tests.len(),
        succeeded,
        tests.len() - succeeded
    );

    Ok(())
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let args = Args::from_args();

    if !args.implementation.exists() {
        eprintln!("implemtation not found");
        std::process::exit(1);
    }

    if !args.tests_root.exists() {
        eprintln!("tests root not found");
        std::process::exit(1);
    }

    let tests = get_tests(&args)?;

    if tests.is_empty() {
        return Ok(());
    }

    let name_len = tests
        .iter()
        .map(|e| {
            e.path()
                .strip_prefix(&args.tests_root)
                .unwrap()
                .as_os_str()
                .len() // Not truly correct, but I know I'm dealing with ASCII, so whatever.
        })
        .max()
        .unwrap();

    if args.generate {
        generate_outputs(&args, &tests, name_len)?;
    } else {
        run_tests(&args, &tests, name_len)?;
    }

    Ok(())
}
