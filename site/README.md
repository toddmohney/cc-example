# Meatbarrrr Frontend

## Viewing the page

Ensure you have the API server running: `stack exec meatbar-exe`

### The super simple version:
Load up the index.html file directly into your browser.
In your browser's location bar, visit `file://{path-to-project-root}/site/public/index.html`

### The slightly less simple version:
If you happen to have Ruby handy on your machine:
```
# install frontend webserver dependencies
bundle install

# startup the server on port 3000
rackup -p 3000

# visit the page
http://localhost:3000
```

## Building the project from source

## Machine Dependencies

npm
browserify

## Building the project from source

```bash
# hop over to the frontend project subdirectory
cd site

# install js packages
npm install

# compile application js
browserify -t [ babelify --presets [ es2015 react ] ] src/main.js -o public/index.js
```
