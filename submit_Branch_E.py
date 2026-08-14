#!/usr/bin/env python3
"""Submit lib/codegen.ml to Branch_E using the GitHub REST API.

This bypasses git/OpenSSL proxy problems and always applies the current local
file on top of the current remote Branch_E tree.

Usage:
  $env:GH_TOKEN = "ghp_..."
  python submit_Branch_E.py
"""
import base64
import json
import os
import sys
import urllib.request
import urllib.error
from pathlib import Path

REPO = "262526ily/pl-c-Project"
BRANCH = "Branch_E"
FILES = ["lib/codegen.ml", "lib/ir.ml", "bin/main.ml"]
COMMIT_MESSAGE = "Add entry-constant propagation across blocks"

API = f"https://api.github.com/repos/{REPO}"


def get_token():
    token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
    if not token and len(sys.argv) > 1:
        token = sys.argv[1]
    if not token:
        print("Set GH_TOKEN or pass the token as the first argument.", file=sys.stderr)
        sys.exit(2)
    return token


def build_opener():
    proxy = os.environ.get("HTTPS_PROXY") or os.environ.get("https_proxy")
    handlers = []
    if proxy:
        handlers.append(urllib.request.ProxyHandler({"http": proxy, "https": proxy}))
    else:
        handlers.append(urllib.request.ProxyHandler())
    return urllib.request.build_opener(*handlers)


def request(opener, token, method, url, data=None):
    body = None
    headers = {
        "Authorization": f"Bearer {token}",
        "Accept": "application/vnd.github+json",
        "User-Agent": "codex-submit",
    }
    if data is not None:
        body = json.dumps(data).encode("utf-8")
        headers["Content-Type"] = "application/json"
    req = urllib.request.Request(url, data=body, headers=headers, method=method)
    try:
        with opener.open(req, timeout=60) as resp:
            raw = resp.read().decode("utf-8")
            if not raw:
                return None
            return json.loads(raw)
    except urllib.error.HTTPError as e:
        detail = e.read().decode("utf-8", errors="replace")
        print(f"HTTP {e.code}: {detail}", file=sys.stderr)
        sys.exit(1)
    except urllib.error.URLError as e:
        print(f"Network error: {e.reason}", file=sys.stderr)
        sys.exit(1)


def main():
    token = get_token()
    opener = build_opener()

    ref = request(opener, token, "GET", f"{API}/git/ref/heads/{BRANCH}")
    old_commit = ref["object"]["sha"]
    print("Current Branch_E:", old_commit)

    commit_info = request(opener, token, "GET", f"{API}/git/commits/{old_commit}")
    old_tree = commit_info["tree"]["sha"]

    tree_items = []
    for file in FILES:
        raw = Path(file).read_bytes()
        # Normalize to LF so OJ builds exactly the same content as the local file.
        normalized = raw.replace(b"\r\n", b"\n")
        encoded = base64.b64encode(normalized).decode("ascii")

        blob = request(opener, token, "POST", f"{API}/git/blobs",
                       {"content": encoded, "encoding": "base64"})
        tree_items.append({"path": file, "mode": "100644",
                           "type": "blob", "sha": blob["sha"]})
        print("blob", file, blob["sha"])

    tree = request(opener, token, "POST", f"{API}/git/trees",
                   {"base_tree": old_tree, "tree": tree_items})
    tree_sha = tree["sha"]

    new_commit = request(opener, token, "POST", f"{API}/git/commits",
                         {"message": COMMIT_MESSAGE,
                          "tree": tree_sha,
                          "parents": [old_commit]})
    new_commit_sha = new_commit["sha"]

    request(opener, token, "PATCH", f"{API}/git/refs/heads/{BRANCH}",
            {"sha": new_commit_sha, "force": True})
    print("Pushed commit:", new_commit_sha)


if __name__ == "__main__":
    main()
