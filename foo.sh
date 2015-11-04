heroku run "curl 'http://thecatapi.com/api/images/get?format=xml' | grep '<url>' | sed 's/<url>//g' | sed 's/<\/url>//g' | sed 's/ *//g'"
