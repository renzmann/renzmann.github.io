"""Static site generator for robbmann.io.

Zero external dependencies. Renders src/ into build/.

Pages: src/pages/<name>.html    -> build/<name>.html
Posts: src/posts/<slug>/index.html + sibling assets
                                -> build/posts/<slug>/index.html + assets
Home:  index page opts in via `posts: all` front matter, which triggers
       {{posts}} to be replaced with a reverse-chron list of post links.
"""

import re
import shutil
import sys
from dataclasses import dataclass
from pathlib import Path

SRC = Path("src")
BUILD = Path("build")


@dataclass
class FrontMatter:
    vars: dict[str, str]
    body: str


def parse_front_matter(path: Path) -> FrontMatter:
    """Split file into (vars_dict, body). Front matter above the --- line."""
    vars_: dict[str, str] = {}
    lines: list[str] = path.read_text(encoding="utf-8").split("\n")
    body_start = 0
    for i, line in enumerate(lines):
        if line.strip() == "---":
            body_start = i + 1
            break
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if ":" in stripped:
            key, _, val = stripped.partition(":")
            vars_[key.strip()] = val.strip()
    body = "\n".join(lines[body_start:])
    return FrontMatter(vars=vars_, body=body)


def slurp(path: Path) -> str:
    text = path.read_text(encoding="utf-8")
    if not text.endswith("\n"):
        text += "\n"
    return text


def expand_includes(text: str, partials_dir: Path) -> str:
    """Expand {{> filename.html}} directives."""
    def replace_include(m: re.Match) -> str:
        before, filename, after = m.group(1), m.group(2), m.group(3)
        return before + slurp(partials_dir / filename) + after
    return re.sub(
        r"^(.*?)\{\{\s*>\s*(\S+)\s*\}\}(.*)$",
        replace_include,
        text,
        flags=re.MULTILINE,
    )


def substitute_vars(text: str, vars_: dict[str, str]) -> str:
    """Replace {{key}} with value from vars. Content first."""
    if "content" in vars_:
        text = re.sub(r"\{\{\s*content\s*\}\}", lambda _: vars_["content"], text)
    for key, val in vars_.items():
        if key == "content":
            continue
        text = re.sub(r"\{\{\s*" + re.escape(key) + r"\s*\}\}", lambda _, v=val: v, text)
    return text


def render_through_layout(front_matter: FrontMatter, body: str) -> str:
    """Wrap a rendered body through the front-matter's layout with partials."""
    vars_ = dict(front_matter.vars)
    vars_["content"] = body
    layout_name = vars_.get("layout", "")
    if not layout_name:
        print(f"error: missing front matter key 'layout'", file=sys.stderr)
        sys.exit(1)
    layout = slurp(SRC / "layouts" / f"{layout_name}.html")
    layout = expand_includes(layout, SRC / "partials")
    out = substitute_vars(layout, vars_)
    out = expand_includes(out, SRC / "partials")
    if not out.endswith("\n"):
        out += "\n"
    return out


def collect_posts() -> list[dict[str, str]]:
    """Return post metadata sorted reverse-chronologically by `date` front-matter."""
    posts_dir = SRC / "posts"
    if not posts_dir.is_dir():
        return []
    posts = []
    for slug_dir in sorted(posts_dir.iterdir()):
        if not slug_dir.is_dir():
            continue
        index = slug_dir / "index.html"
        if not index.is_file():
            continue
        fm = parse_front_matter(index)
        posts.append({
            "slug": slug_dir.name,
            "date": fm.vars.get("date", ""),
            "title": fm.vars.get("title", slug_dir.name),
            "description": fm.vars.get("description", ""),
        })
    posts.sort(key=lambda p: p["date"], reverse=True)
    return posts


def render_post_list(posts: list[dict[str, str]]) -> str:
    """HTML for the homepage post list."""
    items = []
    for p in posts:
        items.append(
            f'  <li class="post-list__item">'
            f'<time class="post-list__date">{p["date"]}</time>'
            f' <a href="posts/{p["slug"]}/">{p["title"]}</a>'
            f"</li>"
        )
    return '<ul class="post-list">\n' + "\n".join(items) + "\n</ul>"


def render_page(page_path: Path, posts: list[dict[str, str]]) -> str:
    """Render a top-level page (src/pages/*.html)."""
    fm = parse_front_matter(page_path)
    body = fm.body
    if fm.vars.get("posts", "").strip() == "all":
        body = body.replace("{{posts}}", render_post_list(posts))
    return render_through_layout(fm, body)


def render_post(post_index: Path) -> str:
    """Render a single post (src/posts/<slug>/index.html)."""
    fm = parse_front_matter(post_index)
    date = fm.vars.get("date", "")
    title = fm.vars.get("title", "")
    body = (
        f'<article class="post">\n'
        f'  <header class="post-meta">\n'
        f'    <h1>{title}</h1>\n'
        f'    <time class="post-meta__date">{date}</time>\n'
        f'  </header>\n'
        f"{fm.body}\n"
        f"</article>\n"
    )
    return render_through_layout(fm, body)


def copy_post_assets(src_slug_dir: Path, dest_slug_dir: Path) -> None:
    """Copy all non-index.html siblings of a post into the build slug dir."""
    for entry in src_slug_dir.iterdir():
        if entry.name == "index.html":
            continue
        dest = dest_slug_dir / entry.name
        if entry.is_dir():
            shutil.copytree(entry, dest, dirs_exist_ok=True)
        else:
            shutil.copy2(entry, dest)


def main() -> None:
    if BUILD.exists():
        shutil.rmtree(BUILD)
    BUILD.mkdir()

    posts = collect_posts()

    for page in sorted((SRC / "pages").glob("*.html")):
        dest = BUILD / page.name
        dest.write_text(render_page(page, posts), encoding="utf-8", newline="\n")

    for post in sorted((SRC / "posts").glob("*/index.html")):
        slug = post.parent.name
        dest_dir = BUILD / "posts" / slug
        dest_dir.mkdir(parents=True, exist_ok=True)
        (dest_dir / "index.html").write_text(
            render_post(post), encoding="utf-8", newline="\n"
        )
        copy_post_assets(post.parent, dest_dir)

    static_src = SRC / "static"
    if static_src.is_dir():
        shutil.copytree(static_src, BUILD / "static", dirs_exist_ok=True)

    cname_src = SRC / "CNAME"
    if cname_src.is_file():
        shutil.copy2(cname_src, BUILD / "CNAME")


if __name__ == "__main__":
    main()
