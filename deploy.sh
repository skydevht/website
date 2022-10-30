#!/bin/sh

unamestr=$(uname)
if [ "$unamestr" = 'Linux' ]; then

  export $(grep -v '^#' .env | xargs -d '\n')

elif [ "$unamestr" = 'FreeBSD' ] || [ "$unamestr" = 'Darwin' ]; then

  export $(grep -v '^#' .env | xargs -0)

fi

zip -r dist.zip dist/

curl -H "Content-Type: application/zip" \
     -H "Authorization: Bearer $NETLIFY_TOKEN" \
     --data-binary "@dist.zip" \
     https://api.netlify.com/api/v1/sites/$NETLIFY_SITE_ID/deploys | jq '.id'

