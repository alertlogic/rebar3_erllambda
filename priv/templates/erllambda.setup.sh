#!/usr/bin/env bash
SCRIPT_DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

# ensure dev_scripts is available and up-to-date
mkdir -p _build
for comp in dev_scripts makeincl; do
    if [[ ! -d _build/${comp} ]]; then
        (cd _build && \
         git clone git@algithub.pd.alertlogic.net:alertlogic/${comp}.git)
    else
        (cd _build/${comp} && \
         git fetch origin && git pull origin master)
    fi
done

setup=_build/dev_scripts/setup.sh
if [[ -e _checkouts/dev_scripts/setup.sh ]]; then
    setup=_checkouts/dev_scripts/setup.sh
fi
SCRIPT_DIR="${SCRIPT_DIR}" COMPS_NEEDED="${COMPS_NEEDED}" \
${setup} "$@"
