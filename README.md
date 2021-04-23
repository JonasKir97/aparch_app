# Simulationsapp für APARCH(1,1)-Prozesse
#### von Jonas Kirchner, entstanden im Rahmen der Masterarbeit an der TU-Dresden.
Dieses Repository beinhaltet alle nötigen Dateien für eine Shiny-Webapp, die lokal im Webbrowser benutzt werden kann.
Voraussetzung für diese App ist eine aktuelle R-Version (https://www.r-project.org/).

### Start der App

### Neu
Um die App zu starten, muss die Datei `appStarter.R` ausgeführt werden. Dazu muss sie heruntergalden und im `RStudio` durchgeklickt werden.

### Alt
Zunächst muss das Repository heruntergeladen werden. Dazu einfach das Repository per git clonen oder als ZIP herunterladen (dazu in diesem Browserfenster über den Dateien auf den grünen Button mit der Aufschrift `Code` klicken und dann `Download ZIP` auswählen) und entpacken.
Bevor die App verwendet wird, müssen alle nötigen Pakete installiert werden. Dazu liegt eine Datei namens `packageManager.R` bereit. Diese muss zunächst gesourct werden. Das kann geschehen indem sie einfach im `RStudio` geöffnet und durch einen Klick auf `source` (oben rechts im Texteditor) gesourct wird. Alternativ lässt sich das ganze auch in der Kommandozeile ausführen. Dazu muss zunächst in den Ordner navigiert werden, indem alle Dateien dieses Repositories liegen. Dann kann man über den Befehl `R` das R-Programm starten. Dann wird die Datei `packageManager.R` über den Befehl `source("packageManger.R")` gesourct. 

Nun können die nötigen Pakete installiert werden. Das geht über den R-Funktionsaufruf `apapp.pm.installMissingPackages()`. Diese Funktion überprüft alle vorhandenen R-Pakete und installiert die noch fehlenden. Das kann einige Minuten dauern. 

Die App wird durch den R-Funktionsaufruf in der Datei `acogarchSimulation.R` gestartet. Um alle benötigten Paktete in der aktuellen R-Session zu laden, muss wieder die Datei `packageManager.R` gesourct werden (siehe Vor der ersten Benutzung). Dann müssen die Funktionen `apapp.pm.loadRequiredPackages()` (laden der benötigten Pakete) und `apapp.pm.sourceNeededFiles(gitDirectoryPath = "Pfad/Zum/Heruntergaldenen/GitRepository")` (Sourcen der benötigten Dateien) aufgerufen werden. Dann liegen alle benötigten Funktionen in der Umgebung der aktuellen R-Session und die App kann mit dem Aufruf `aCOGARCH_SimulationApp()` gestartet werden. Die App läuft dann lokal auf dem "localhost" (127.0.0.1) unter Port 2021. Falls dieser Port durch bereits laufende Anwendungen belegt ist, so kann dieser abgeändert werden, indem in der Datei 'acogarchSimulation.R' in der vorletzten Zeile das `port`-Argument in der `options`-Liste geändert wird.

### Funktionalität der App
Die App biete folgende Funktionalität... TODO.

#### Simulationen
In der App können zeitdiskrete und zeitstetige APARCH(1,1)-Modelle simuliert werden. 
Bisher ist nur das zeitdiskrete Modell implmentiert. Die Parameter der Simulation können in der Sidebar eingestellt werden.
Das sind
- Länge der Simulation: Die Anzahl der Punkte, die simuliert werden
- sämtliche Modellparameter: delta,gamma,Theta,alpha,beta
- Seed: ein Seed, der vor dem zufälligen Erzeugen der Noises gesetzt wird, um Reproduzierbarkeit zu gewährleisten

#### Schätzung
In der Schätzung kann ein Datensatz aus den historischen Daten der Webseite nasdaq.com geschätzt werden. Die Daten des S&P500 gibt es z.B. unter https://www.nasdaq.com/market-activity/index/spx/historical als csv-Download. Inder Suchleiste `Search for Historical Data` können beliebige Titel gesucht werden. Sodann klickt man in der Übersicht auf den gewünschten Zeitraum (a betsen `MAX`) und auf den `Download Data`-Button. Testweise stehen im Repository im Ordner `nasdaqCsvExampleData` drei csv-Dateien zur Verfügung: Tesla, S&P500, LVMH. 
Die Parameterschätzung ist bisher noch nicht implementiert. Wenn man einen Datensatz importiert, so werden bisher erstmal nur der Verlauf der Preise und die Log-Returns über den gesamten Zeitraum dargestellt. 

### Inhalt des Repositories
| Datei | Beschreibung |
| ------ | ------ |
| `packageManager.R` | Funktionen für das Handling der benötigten Packages |
| `acogarchSimulationAppHelpers.R` | Hilfsfunktionen für die App, wie z.B. Simulationsfunktionen |
| `acogarchSimulation.R` | Kernfunktion zum Starten der App |
| `guiHelpers.R` |Hilfsfunktionen für die GUI |
| `plotHelpers.R` | Hilfsfunktionen für Plots |
| `simulateDiscreteAPARCH11inCPP.cpp` | C++ Funktion zur sehr schnellen Berechnung einer diskreten APARCH(1,1)-Simulation |
| `nasdaqDataReader.R` | Hilfsfunktionen um csv-Export von nasdaq.com, z.B. https://www.nasdaq.com/market-activity/index/spx/historical für S&P500 |
| `nasdaqCsvExampleData` | Verzeichnis mit Beispiel-csv-Exports von nasdaq.com: Tesla,S&P500,LVMH |
