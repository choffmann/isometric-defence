# Abschlussprojekt

Bitte an dieser Stelle eine kurze Beschreibung hinzufügen, wie die Anwendung gestartet bzw. ggf. übersetzt wird.

Zuerst muss das Elm-Programm transpiliert werden mit:
```bash
elm make src/Main.elm --output=public/elm.js
```

Im Anschluss muss die index.html im Browser geöffnet werden.

<s>Da keine externen Ressourcen angefordert werden reicht ein einfaches drag and drop in den Browser.</s>

Da ein Image durch Elm geladen werden muss benötigt man einen Webserver, hier eignet sich zum Beispiel 
der in Python integrierte Webserver der mit

```bash
cd public
python3 -m http.server
```

gestartet wird.

Dann ist das Spiel über `http:localhost:8000/` erreichbar.

Unser Spiel ist ein Tower-Defense Spiel wo Türme auf Evil Boxes schießen.
Es gibt 7 Runden mit insgesamt 7 verschiedene Gegnertypen, darunter ein Boss, und 5 verschiedene Türme.
Unser Spiel verfügt dabei über zwei verschiedene Ansichten (TopDown und Isometrisch).

Zudem benutzen wir Ports um einen FullscreenMode zu ermöglichen.