#!/bin/bash

SHIP="zod"
CODE="lidlut-tabwed-pillex-ridrup"
BASE_URL="localhost:8081"

auth_res=$(curl -i -X POST --silent -d "password=$CODE" "$BASE_URL/~/login")
cookie_header="$(echo "$auth_res" | grep -i 'set-cookie:' | awk '{print $2}' | sed 's/.$//')"
message_id=0

subscribe() {
    curl -X PUT \
         --cookie "$cookie_header" \
         -d "[{
             \"id\":$message_id,
             \"action\":\"delete\"
             }]" \
         "$BASE_URL/~/channel/homunculus"
    curl -X PUT \
         --cookie "$cookie_header" \
         -d "[{
             \"id\":$message_id,
             \"action\":\"subscribe\",
             \"ship\":\"$SHIP\",
             \"app\":\"homunculus\",
             \"path\":\"/homunculus-http\"
             }]" \
         "$BASE_URL/~/channel/homunculus"
}

poke() {
    ((message_id++))
    curl -X PUT \
         --cookie "$cookie_header" \
         -d "[{
             \"id\":$message_id,
             \"action\":\"poke\",
             \"ship\":\"$SHIP\",
             \"app\":\"homunculus\",
             \"mark\":\"json\",
             \"json\":\"$1\"
             }]" \
         "$BASE_URL/~/channel/homunculus"
}

ack() {
    ((message_id++))
    curl -X PUT \
         --cookie "$cookie_header" \
         -d "[{
             \"id\":$message_id,
             \"action\":\"ack\",
             \"event-id\":$1
             }]" \
         "$BASE_URL/~/channel/homunculus"
}

stream() {
    local event_id=0
    curl -N --silent \
         -H "Accept:text/event-stream" \
         --cookie "$cookie_header" \
         "$BASE_URL/~/channel/homunculus" |
    while read -r -s line; do
        if [[ "${line:0:4}" = "id: " ]]; then
            event_id="${line:4}"
        elif [[ "${line:0:6}" = "data: " ]] &&  grep -q "json" <<< "$line"; then
            dat=$(echo "${line:6}" | jq -r '.json')
            echo -e -n "$dat"
            ack $event_id
        fi
    done
}

read_input() {
    while read -r -s -N 1 char; do
        if [[ "$char" = $'\033' ]] || [[ "$char" =~ [[:cntrl:]] ]]; then
            read -r -s -t 0.01 chars
            printf -v seq "%q" "$char$chars"
            if [[ "${seq:0:1}" = '$' ]]; then
                eval sequence=${seq:1}
            else
                eval sequence=$seq
            fi
            poke "\\$sequence"
        elif [[ "$char" = '\' ]] || [[ "$char" = '"' ]]; then
            poke "\\$char"
        else
            poke "$char"
        fi
    done
}

PS1=

subscribe

stream &
pid[0]=$!
trap "kill ${pid[0]}; exit 1" INT

read_input