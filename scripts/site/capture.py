#!/usr/bin/env python3
"""Record what the site quotes by building Crossbow and running it.

    python3 scripts/site/capture.py

Builds GHC with crossbow's dependencies (ghc-env.nix), builds the interpreter offline with cabal,
then runs each example in EXAMPLES as a one-line program in its own process (an error in a
builtin ends the interpreter, so one failing example must not hide the others), and runs test/aoc.cb,
recording only how many results it prints and how long it takes: the results are puzzle answers
derived from personal inputs, so they are not published.

Writes scripts/site/data/{examples.json, aoc.json}.
"""
import json
import os
import subprocess
import sys
import tempfile
import time
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
DATA = HERE / "data"

EXAMPLES = [
    "1:10 | filter (even) | sum",
    "[+ 1, + 2] | [3, 4]",
    "{$0 | sum | + $1} | 1:10 | 5",
    "enum 1:10",
    "[[1,2],[3,4]] | map (`+)",
    "[[1,2],[3,4]] | sum!",
    "[1, \"hi\", [True]]",
    "_2 [10, 20, 30]",
    "1:3 | fork 2",
    "pairs 1:5",
    "windows 3 1:6",
    "[3, 5, 4, 6, 9] | pairs",
    "[3, 5, 4, 6, 9] | pairs | count `<",
    "[3, 5, 4, 6, 9] | windows 3",
    "[3, 5, 4, 6, 9] | windows 3 | sum!",
    "[3, 5, 4, 6, 9] | windows 3 | sum! | pairs | count `<",
    "nosuchthing 3",
    "filter (even) (sum 1:10)",
]


def main():
    DATA.mkdir(exist_ok=True)
    with tempfile.TemporaryDirectory() as tmp:
        env = {"PATH": os.environ.get("PATH", "/usr/bin:/bin"), "HOME": os.path.join(tmp, "home"),
               "LANG": "C.UTF-8", "LC_ALL": "C.UTF-8"}
        ghc = os.path.join(tmp, "ghc")
        subprocess.run(["nix", "build", "--impure", "-f", str(HERE / "ghc-env.nix"), "-o", ghc], check=True, env=env)
        env["PATH"] = os.path.join(ghc, "bin") + ":" + env["PATH"]
        build = os.path.join(tmp, "dist")
        subprocess.run(["cabal", "build", "--offline", "-w", os.path.join(ghc, "bin", "ghc"), "--builddir", build,
                        "exe:crossbow"], cwd=ROOT, check=True, env=env, capture_output=True)
        exe = subprocess.run(["find", build, "-type", "f", "-name", "crossbow", "-perm", "-u+x"],
                             capture_output=True, text=True).stdout.split()[0]
        work = os.path.join(tmp, "work")
        os.makedirs(work)
        Path(work, "lib.cb").write_text((ROOT / "lib.cb").read_text())  # lib.cb is loaded from the cwd
        examples = []
        for e in EXAMPLES:
            r = _run(exe, e, work, env)
            examples.append({"input": e, **r})
            print(e, "=>", r, flush=True)
        t = time.time()
        r = subprocess.run([exe, "test/aoc.cb"], cwd=ROOT, env=env, capture_output=True, text=True, timeout=600)
        aoc = {"results": len([l for l in r.stdout.splitlines() if l.strip()]), "exit": r.returncode,
               "seconds": round(time.time() - t, 1), "errors": "rror" in (r.stdout + r.stderr)}
    (DATA / "examples.json").write_text(json.dumps(examples, indent=1, ensure_ascii=False) + "\n")
    (DATA / "aoc.json").write_text(json.dumps(aoc, indent=1) + "\n")
    print("aoc.cb:", aoc)


def _run(exe, expr, work, env):
    """Run one expression as a one-line program in its own process (crossbow FILE prints each
    line's result). An error inside a builtin ends the interpreter, so it must not share a process."""
    src = os.path.join(work, "example.cb")
    Path(src).write_text(expr + "\n")
    r = subprocess.run([exe, src], cwd=work, env=env, capture_output=True, text=True, timeout=30)
    out = [l for l in (r.stdout + r.stderr).splitlines() if l.strip()]
    crashed = r.returncode != 0
    return {"output": next((l for l in out if "crossbow:" in l), out[0] if out else "") if crashed else (out[0] if out else ""),
            "crashed": crashed}


if __name__ == "__main__":
    main()
