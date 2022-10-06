#!/bin/bash

ulimit -s unlimited

function print_usage_info {
    echo "Usage: ${0##*/} [OPTION]... [--] [DFLOWFMOPTIONS]..."
    echo "Run dflowfm program."
    echo
    echo
    echo "Options for ${0##*/}:"
    echo "-h, --help"
    echo "       print this help message and exit"
    echo
    echo "--"
    echo "       All following arguments are passed to D-Flow FM."
    echo "       (optional)"
    echo "Options for D-Flow FM, see dflowfm --help"

    exit 1
}

# Estimates the number of physical cores based on /proc/cpuinfo (not counting hyperthreading)
# If not found variable $numcores remains empty.
function estimate_num_cores {
    corespercpu=$(grep 'cpu cores' /proc/cpuinfo 2>/dev/null | head -1 | sed -e 's/.*:[ ]*\([0-9]*\)/\1/')
    numcpu=$(grep 'physical id' /proc/cpuinfo 2>/dev/null | sort | uniq | wc -l)

    if [ ! -z "$corespercpu" ] && [ ! -z "$numcpu" ]; then
        numcores=$(( ${numcpu}*${corespercpu} ))
    fi
}

function set_omp_threads {
    estimate_num_cores

    if [ ! -z "${OMP_NUM_THREADS}" ]; then
        # Do nothing: someone has already set OMP_NUM_THREADS
        :
    #elif [ ! -z "$numcores" ]; then
        # Use numcores-1 (if multiple cores at all)
        # Set OMP_NUM_THREADS variable in current process (no need to export)
    #    OMP_NUM_THREADS=$((numcores > 2 ? numcores-1 : 1))
    else
        # Could not determine numcores, leave OMP_NUM_THREADS empty.
        :
    fi
}



while [[ $# -ge 1 ]]
do
key="$1"
shift
case $key in
    -h|--help)
    print_usage_info
    ;;
    --)
    dfmoptions=$*
    break	# exit loop, all remaining options to dflowfm executable
    ;;
    *)
    dfmoptions="$key $*"
    break	# exit loop, $key+all remaining options to dflowfm executable
    ;;
esac
done

scriptdirname=`readlink \-f \$0`
scriptdir=`dirname $scriptdirname`
export D3D_HOME=$scriptdir/.. 
export LD_LIBRARY_PATH=$D3D_HOME/lib:$LD_LIBRARY_PATH

module load intelmpi/21.2.0 &>/dev/null
export FI_PROVIDER=tcp

set_omp_threads

echo "$D3D_HOME/bin/dflowfm --nodisplay --autostartstop $dfmoptions"
#ldd $D3D_HOME/bin/dflowfm
$D3D_HOME/bin/dflowfm --nodisplay --autostartstop $dfmoptions


