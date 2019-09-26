# URL="http://simple-serb.herokuapp.com"
URL="http://localhost:8080"

declare -A methods

methods=( ["send"]="POST" ["rec"]="DELETE" )

if [[ "$#" -lt 2 || ( $1 == "send" && $# -ne 3 )  ]]; then
    printf "Usage:\tclient.sh send addr message\n\tclient.sh rec addr"
    exit 1
fi

if [[ "$1" == "send" ]]; then
    curl -GX ${methods[$1]} --data-urlencode addr="$2" --data-urlencode message="$3" "${URL}"
elif [[ "$1" == "rec" ]]; then
    curl -X ${methods[$1]} "${URL}/?addr=$2"
else
    printf "Usage:\tclient.sh send addr message\n\tclient.sh rec addr"
    exit 1
fi
