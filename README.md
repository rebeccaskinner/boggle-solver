# Running

The elm application is already built (frontend/index.html) or you can build it
with `make` from the `frontend` directory.

Follow the directions in [The server's readme file](server/README.md) for
information about building the server.

From the `server` directory run the application with:

```
stack exec rstudio-exercise -- /usr/share/dict/words ../frontend/index.html
```

then go to `http://localhost:8080` in your browser.

# Limitations

The frontend only supports a 4x4 boggle grid and doesn't provide useful error
messages, a usable UI, accessibility, etc.
