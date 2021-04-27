# Simulationsapp für APARCH(1,1)-Prozesse
#### von Jonas Kirchner, entstanden im Rahmen der Masterarbeit an der TU-Dresden.
Dieses Repository beinhaltet alle nötigen Dateien für eine Shiny-Webapp, die lokal im Webbrowser benutzt werden kann.
Voraussetzung für diese App ist eine aktuelle R-Version (https://www.r-project.org/).

### Start der App
Es wird eine bestehende Internetverbindung vorausgesetzt. Um die App zu starten, muss die Datei `appStarter.R` ausgeführt werden. Dazu muss sie heruntergeladen und im `RStudio` durchgeklickt werden, oder man startet sie von der Kommandozeile (Die Kommandozeile wird beispielsweise geöffnet mit: Windows-Taste + R, dann `cmd` eingeben, Bestätigen mit `Enter`) über den Befehl `RScript Pfad/Zur/appStarter.R`.

### Funktionalität der App

#### APARCH-Simulationen
In der App können zeitdiskrete und zeitstetige APARCH(1,1)-Modelle simuliert werden. 
Bisher ist nur das zeitdiskrete Modell implmentiert. 
##### Diskrete Simulation
Die sikrete Simulation des APARCH(1,1) läuft mit N(0,1)-verteilten Noises. Das kann noch auf andere Arten von Noises erweitert werden. Die Parameter der Simulation können in der Sidebar eingestellt werden.
Diese sind
- Länge der Simulation: Die Anzahl der Punkte, die simuliert werden
- sämtliche Modellparameter: delta,gamma,Theta,alpha,beta
- Seed: ein Seed, der vor dem zufälligen Erzeugen der Noises gesetzt wird, um Reproduzierbarkeit zu gewährleisten
##### Stetige Simulation
...


#### Schätzung des ACOARCH
In der Schätzung kann ein Datensatz aus den historischen Daten der Webseite nasdaq.com geschätzt werden. Die Daten des S&P500 gibt es z.B. unter https://www.nasdaq.com/market-activity/index/spx/historical als csv-Download. In der Suchleiste `Search for Historical Data` können beliebige Titel gesucht werden. Sodann klickt man in der Übersicht auf den gewünschten Zeitraum (am besten `MAX`) und auf den `Download Data`-Button. Testweise stehen im Repository im Ordner `nasdaqCsvExampleData` drei csv-Dateien zur Verfügung: Tesla, S&P500, LVMH. 
Die Parameterschätzung ist bisher noch nicht implementiert. Wenn man einen Datensatz importiert, so werden bisher erstmal nur der Verlauf der Preise und die Log-Returns über den gesamten Zeitraum dargestellt. 


#### Lévysimulation
In der Lévysimulation können verschiedene Lévyprozesse simuliert werden. Diese sind
- Compound Poisson
- Varianz-Gamma
- Brownsche Bewegung

Die entsprechenden Parameter zur Spezifikation der Prozesse können in der Sidebar eingestellt werden.
Zudem kann in der Simulation eine First-Jump-Approximation durchgeführt werden. Deer Button dafür wird sichtbar, sobald ein Lévyprozess simuliert wurde.



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
| `firstJumpApproximation.R` | enthält Funktion für die First-Jump-Approximation eines Lévyprozesses |
