# Simulationsapp für APARCH(1,1)-Prozesse
#### Entstanden im Rahmen meiner Masterarbeit
###
Dieses Repository beinhaltet alle nötigen Dateien für eine Shiny-Webapp, die lokal im Webbrowser benutzt werden kann.
Voraussetzungen für diese App ist eine aktuelle R-Version (https://www.r-project.org/).

### Vor der ersten Benutzung
Zunächst muss das Repository heruntergeladen werden. Dazu einfach das Repository per git clonen oder als ZIP herunterladen und entpacken.
Bevor die App verwendet wird, müssen alle nötigen Pakete installiert werden. Dazu liegt eine Datei namens `packageManager.R` bereit. Diese muss zunächst gesourct werden. Das kann geschehen indem sie einfach im `RStudio` geöffnet und durch einen Klick auf `source` (oben rechts im Texteditor) gesourct wird. Alternativ lässt sich das ganze auch in der Kommandozeile ausführen. Dazu muss zunächst in den Ordner navigiert werden, indem alle Dateien dieses Repositories liegen. Dann kann man über den Befehl `R` das R-Programm starten. Dann wird die Datei `packageManager.R` über den Befehl `source("packageManger.R")` gesourct. 

Nun können die nötigen Pakete installiert werden. Das geht über den R-Funktionsaufruf `installMissingPackages()`. Diese Funktion überprüft alle vorhandenen R-Pakete und installiert die noch fehlenden. Das kann einige Minuten dauern. 

### Start der App
Die App wird durch den R-Funktionsaufruf in der Datei `acogarchSimulation.R` gestartet. Um alle benötigten Paktete in der aktuellen R-Session zu laden, muss wieder die Datei `packageManager.R` gesourct werden (siehe Vor der ersten Benutzung). Dann müssen die Funktionen `loadRequiredPackages()` (laden der benötigten Pakete) und `sourceNeededFiles(gitDirectoryPath = "Pfad/Zum/Heruntergaldenen/GitRepository")` (Sourcen der benötigten Dateien) aufgerufen werden. Dann liegen alle benötigten Funktionen in der Umgebung der aktuellen R-Session und die App kann mit dem Aufruf ´´´aCOGARCH_SimulationApp()´´´ gestartet werden. Die App läuft dann lokal auf dem "localhost" (127.0.0.1) unter Port 2021. Falls dieser Port durch bereits laufende Anwendungen belegt ist, so kann dieser abgeändert werden, indem in der Datei 'acogarchSimulation.R' in der vorletzten Zeile das `port`-Argument in der `options`-Liste geändert wird.

### Funktionalität der App
Die App biete folgende Funktionalität. Simulationen ... TODO.

### Inhalt des Repositories
| Datei | Beschreibung |
| ------ | ------ |
| `packageManager.R` | Funktionen für das Handling der benötigten Packages |
| `acogarchSimulationAppHelpers.R` | Hilfsfunktionen für die App, wie z.B. Simulationsfunktionen |
| `acogarchSimulation.R` | Kernfunktion zum Starten der App |
| `simulateDiscreteAPARCH11inCPP.cpp` | C++ Funktion zur sehr schnellen Berechnung einer diskreten APARCH(1,1)-Simulation |