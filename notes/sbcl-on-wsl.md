# Installing and Running SBCL under WSL

This guide covers installing Windows Subsystem for Linux (WSL), then installing SBCL (Steel Bank Common Lisp) inside it, along with Quicklisp and a comfortable REPL setup.

## 1. Install WSL

Open **PowerShell as Administrator** and run:

```powershell
wsl --install
```

This installs WSL2 and defaults to an Ubuntu distribution. Reboot if prompted.

If WSL is already installed and you just want Ubuntu specifically:

```powershell
wsl --install -d Ubuntu
```

Check your installed distros and versions:

```powershell
wsl -l -v
```

Make sure the distro shows `VERSION 2`. If it shows version 1, convert it:

```powershell
wsl --set-version Ubuntu 2
```

After installation, launch Ubuntu from the Start menu. The first launch will ask you to create a Linux username and password (separate from your Windows login).

## 2. Update the Ubuntu environment

Inside the Ubuntu terminal:

```bash
sudo apt update && sudo apt upgrade -y
```

## 3. Install SBCL

The simplest path is via apt:

```bash
sudo apt install sbcl -y
```

Verify it works:

```bash
sbcl --version
```

**Note:** Ubuntu's apt repository often has an older SBCL release. If you want the latest version, download it directly from [sbcl.org](https://www.sbcl.org/platform-table.html) or build from source. For most day-to-day Lisp work, the apt version is fine to start with.

## 4. Install Quicklisp

Quicklisp is the standard library/package manager for Common Lisp.

```bash
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp
```

At the SBCL REPL that loads, run:

```lisp
(quicklisp-quickstart:install)
(ql:add-to-init-file)
(sb-ext:exit)
```

`ql:add-to-init-file` makes Quicklisp load automatically every time you start SBCL.

## 5. Quality-of-life improvements

### rlwrap (better line editing at the REPL)

SBCL's raw REPL has clunky line editing. `rlwrap` gives you readline-style history and editing:

```bash
sudo apt install rlwrap -y
```

Then run SBCL as:

```bash
rlwrap sbcl
```

You can alias this permanently by adding to `~/.bashrc`:

```bash
echo "alias sbcl='rlwrap sbcl'" >> ~/.bashrc
source ~/.bashrc
```

### Emacs + SLIME (recommended for serious development)

For a full interactive development experience:

```bash
sudo apt install emacs -y
```

Then install SLIME via Quicklisp or `package.el`/`use-package` in your Emacs config, and set SBCL as the inferior Lisp:

```elisp
(setq inferior-lisp-program "sbcl")
```

WSL runs a full Linux environment, so Emacs, SLIME, and SBCL behave exactly as they would on native Linux — no special WSL-specific configuration is needed here.

## 6. Running SBCL

- Start a REPL: `sbcl` (or `rlwrap sbcl`)
- Load a file: `sbcl --load myfile.lisp`
- Run a script and exit: `sbcl --script myfile.lisp`
- Exit the REPL: `(sb-ext:exit)` or Ctrl+D

## 7. File access between Windows and WSL

Your Windows drives are mounted under `/mnt/c/`, `/mnt/d/`, etc. Your Linux home directory (`~`, i.e. `/home/username`) is a separate, faster filesystem — for Lisp projects, it's best to keep source files there rather than under `/mnt/c/...` to avoid file-system performance issues.

To open Windows Explorer to your current WSL directory:

```bash
explorer.exe .
```

## Summary

| Step | Command |
|---|---|
| Install WSL | `wsl --install` |
| Update packages | `sudo apt update && sudo apt upgrade -y` |
| Install SBCL | `sudo apt install sbcl -y` |
| Install rlwrap | `sudo apt install rlwrap -y` |
| Run REPL | `rlwrap sbcl` |
| Install Quicklisp | see Section 4 |
