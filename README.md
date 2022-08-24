# Abschlussprojekt

Bitte an dieser Stelle eine kurze Beschreibung hinzufügen, wie die Anwendung gestartet bzw. ggf. übersetzt wird.

```bash
elm make src/Main.elm --output=public/elm.js
```

Im Anschluss muss die index.html im Browser geöffnet werden.
<s>Da keine externen Ressourcen angefordert werden reicht ein einfaches drag and drop in den Browser.</s>
Da ein Image geladen durch elm werden muss benötigt man einen webserver, hier eignet sich zum Beispiel 
der in Python integrierte Webserver der mit

```bash
cd public
python3 -m http.server
```

gestartet wird dann ist das game über `http:localhost:8000/` erreichbar.
