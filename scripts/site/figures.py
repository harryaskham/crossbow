"""Figures and generated regions for the Crossbow site, from data captured by running Crossbow."""
import html
import json
from pathlib import Path

from diagrams import graph

DATA = Path(__file__).resolve().parent / "data"
METRICS = {"sans": 1.0, "mono": 0.6}
ROOT = DATA.parents[2]


def load(name):
    return json.loads((DATA / name).read_text())


def ex(expr):
    for e in load("examples.json"):
        if e["input"] == expr:
            return e["output"]
    raise KeyError(expr)


def pipeline():
    xs = "[3, 5, 4, 6, 9]"
    pairs = ex(f"{xs} | pairs")
    count = ex(f"{xs} | pairs | count `<")
    wins = ex(f"{xs} | windows 3")
    sums = ex(f"{xs} | windows 3 | sum!")
    final = ex(f"{xs} | windows 3 | sum! | pairs | count `<")
    return graph(
        "pipeline", "A pipeline, stage by stage",
        f"Two pipelines over the list {xs}, with each intermediate value as Crossbow prints it. The first, "
        f"pairs then count with a backtick-uncurried <, turns the list into neighbouring pairs {pairs} and counts the "
        f"pairs that increase: {count}. The second takes sliding windows of three, {wins}, sums each with sum! "
        f"(map sum), {sums}, and then counts increases the same way: {final}.",
        940, 260,
        nodes=[
            dict(id="a", x=100, y=70, label=xs, kind="ink", mono=True, w=170),
            dict(id="p", x=380, y=70, label=pairs, sub="| pairs", kind="b", mono=True, w=280),
            dict(id="c", x=700, y=70, label=count, sub="| count `<", kind="accent", mono=True, w=150),
            dict(id="b", x=100, y=190, label=xs, kind="ink", mono=True, w=170),
            dict(id="w", x=360, y=190, label=wins, sub="| windows 3", kind="b", mono=True, w=250),
            dict(id="s", x=620, y=190, label=sums, sub="| sum!", kind="b", mono=True, w=150),
            dict(id="f", x=850, y=190, label=final, sub="| pairs | count `<", kind="accent", mono=True, w=150),
        ],
        edges=[{"from": "a", "to": "p", "kind": "ink"}, {"from": "p", "to": "c", "kind": "ink"},
               {"from": "b", "to": "w", "kind": "ink"}, {"from": "w", "to": "s", "kind": "ink"},
               {"from": "s", "to": "f", "kind": "ink"}],
    )


def runtime():
    return graph(
        "runtime", "How a line runs",
        "How the interpreter runs a line. The parser, written with Parsec, splits the line into clauses at each | and "
        "parses each clause into values: numbers, strings, lists, ranges, functions with some arguments supplied, lambdas "
        "in braces whose arguments are $0, $1 and so on, f! for map, _n for indexing, and `f for a function taking one "
        "list of arguments. The evaluator runs the clauses left to right, feeding each result into the next clause as its "
        "next argument. Every name is looked up in one table: it starts with the built-in functions written in Haskell, and "
        "each binding made with <- adds a name or replaces one, beginning with the standard library lib.cb, which is "
        "itself Crossbow.",
        940, 290,
        nodes=[
            dict(id="src", x=95, y=80, label="a line of text", sub="1:10 | filter (even) | sum", kind="file", w=180),
            dict(id="parse", x=320, y=80, label="parser", sub="Parsec: clauses split at |", kind="ink", w=190),
            dict(id="vals", x=560, y=80, label="clauses of values", sub="functions, lambdas, lists…", kind="b", w=200),
            dict(id="eval", x=800, y=80, label="evaluator", sub="each result → next clause", kind="accent", w=210),
            dict(id="hs", x=330, y=225, label="Haskell builtins", sub="the table starts here", kind="muted", w=210),
            dict(id="lib", x=560, y=225, label="<- bindings", sub="lib.cb, then yours", kind="c", w=200),
            dict(id="tab", x=800, y=225, label="one name table", sub="a binding adds or replaces", kind="b", w=210),
        ],
        edges=[{"from": "src", "to": "parse", "kind": "ink"}, {"from": "parse", "to": "vals", "kind": "ink"},
               {"from": "vals", "to": "eval", "kind": "ink"},
               {"from": "eval", "to": "tab", "kind": "b", "out": "b", "in": "t", "label": "names", "at": 0.5},
               {"from": "hs", "to": "lib", "kind": "muted"},
               {"from": "lib", "to": "tab", "kind": "c"}],
    )


FIGS = {"pipeline": pipeline, "runtime": runtime}


def examples():
    rows = []
    notes = {
        "1:10 | filter (even) | sum": "a pipeline: each clause takes the previous result",
        "[+ 1, + 2] | [3, 4]": "a list of functions applies element-wise",
        "{$0 | sum | + $1} | 1:10 | 5": "a lambda with two arguments",
        "enum 1:10": "enum, from the standard library",
        "[[1,2],[3,4]] | map (`+)": "`+ takes one list of two arguments",
        "[[1,2],[3,4]] | sum!": "sum! is map sum",
        "[1, \"hi\", [True]]": "lists may mix types",
        "_2 [10, 20, 30]": "_n takes the item at index n",
        "1:3 | fork 2": "fork copies a value",
        "nosuchthing 3": "an unknown name is a clean error",
    }
    for e in load("examples.json"):
        if e["input"] not in notes:
            continue
        rows.append(f'<tr><td><code>{html.escape(e["input"])}</code></td><td><code>{html.escape(e["output"])}</code></td>'
                    f'<td class="note">{html.escape(notes[e["input"]])}</td></tr>')
    return ('<table class="ex"><caption>Each line run by the interpreter (scripts/site/capture.py)</caption>'
            '<thead><tr><th scope="col">Input</th><th scope="col">Output</th><th scope="col"></th></tr></thead><tbody>'
            + "".join(rows) + "</tbody></table>")


def crash():
    e = next(x for x in load("examples.json") if x["crashed"])
    return (f'<pre tabindex="0"><code><span class="p">{html.escape(e["input"])}</span>\n'
            f'<span class="e">{html.escape(e["output"])}</span></code></pre>')


def source(path, start=None, end=None):
    text = (ROOT / path).read_text()
    lines = text.splitlines()
    sel = lines[start:end] if start is not None else lines
    return lambda: '<pre tabindex="0"><code>' + html.escape("\n".join(sel).rstrip()) + "</code></pre>"


def aoc_note():
    a = load("aoc.json")
    return (f'<p class="note">Running this file prints {a["results"]} answers in {a["seconds"]} seconds '
            f'(the answers depend on personal puzzle inputs, so they are not shown).</p>')


GEN = {
    "examples": examples,
    "crash": crash,
    "aoc-2021": source("test/aoc.cb", 2, 33),
    "lib": source("lib.cb"),
    "aoc-note": aoc_note,
}
