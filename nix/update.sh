#!/usr/bin/env nix-shell
#!nix-shell -i bash -p bash nix curl jq --pure

set -euo pipefail

update() {
    local versions="$1"
    local package="$2"
    echo "Updating $package"
    local owner=$(jq -r ".[\"$package\"].owner" $versions)
    local repo=$(jq -r ".[\"$package\"].repo" $versions)
    local branch=$(jq -r ".[\"$package\"].branch" $versions)
    local rev=$(jq -r ".[\"$package\"].rev" $versions)

    echo "Repository: $owner/$repo"
    echo "Branch: $branch"

    case "$PREFETCH_ONLY" in
        TRUE)
            local new_rev=$rev
            echo "Using existing revision"
            ;;
        FALSE)
            local new_rev=$(curl -sfL \
                                 https://api.github.com/repos/$owner/$repo/git/refs/heads/$branch \
                                | jq -r .object.sha)
            echo "Fetching latest revision on branch $branch"
            ;;
    esac
    echo "The following revision will be used: $new_rev"

    local url=https://github.com/$owner/$repo/archive/$new_rev.tar.gz
    local new_sha256=$(nix-prefetch-url --unpack "$url")

    res=$(cat $versions \
              | jq -rM ".[\"$package\"].rev = \"$new_rev\"" \
              | jq -rM ".[\"$package\"].sha256 = \"$new_sha256\""
       )

    echo "New versions file:"
    echo "$res" | tee $versions
}

ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )"/.. >/dev/null && pwd )"
VERSIONS_DEF="$ROOT/nix/versions.json"
PACKAGES_DEF="$(cat $VERSIONS_DEF | jq -r 'keys | .[]')"

VERSIONS="$VERSIONS_DEF"
echo "Using versions file ${VERSIONS}"

PREFETCH_ONLY="FALSE"
PACKAGES=""

while [[ $# -gt 0 ]]; do
    case "$1" in
        --prefetch)
            PREFETCH_ONLY="TRUE"
            shift
            ;;
        *)
            PACKAGES="$1 $PACKAGES"
            shift
            ;;
    esac
done

if [ -z "$PACKAGES" ]; then
    PACKAGES="$PACKAGES_DEF"
fi

echo "Updating packages: $PACKAGES"

for p in $PACKAGES; do
    echo
    update "$VERSIONS" "$p"
done
