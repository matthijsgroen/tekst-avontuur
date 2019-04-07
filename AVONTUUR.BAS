DECLARE SUB Muteer (slot%, op$, value%)
DECLARE SUB VoerActieUit (actie$)
DECLARE SUB ToonActies ()
DECLARE FUNCTION Toets! (slot%, op$, value%)
DECLARE FUNCTION Toegestaan% (predicate$)
DECLARE SUB ToonGebeurtenis ()
DECLARE SUB Tekst (Kleur%, Regel$)
DECLARE FUNCTION Interpoleer$ (Tekst$)

' Zorg dat we spel kunnen afsluiten met ESC
KEY 15, CHR$(0) + CHR$(1) ' ESC
KEY(15) ON
ON KEY(15) GOSUB Stoppen

KEY 16, CHR$(0) + CHR$(31) ' S
ON KEY(16) GOSUB Opslaan
ON ERROR GOTO Foutafhandeling

' Gebeurtenissen
' 0 = Spel gewonnen (1), Bezig(0), Verloren(2)
' 1 = location
'   0 = Proloog
'   1 = Bos
'   2 = Akkers
'   3 = Heuvels
'   4 = Moeras
'   5 = Intro
'   6 = Dorp
'   7 = Donker woud
'   8 = Rivier
' 2 = location state 0 = look, 1 = Open inventory, 2 = empty
' 3 = Travel state
'   1 = arrive by foot from north
'   2 = arrive by foot from east
'   3 = arrive by foot from south
'   4 = arrive by foot from west
' 4 = Proloog state
' 5 = Proloog retries

' 20 = hasBag
' 21 = hasBranch
' 22 = money
' 23 = prolog key / food

' 0. Proloog
1 DATA "1=0;4=0"
DATA "*c2", "Er was eens, lang geleden een stoere koerier..."
DATA "$Z naam was $n", ""
DATA "$H was altijd super snel in het bezorgen van brieven en pakketjes."
DATA "Dingen bezorgen was dan ook $z lust en $z leven."
DATA "Op het moment dat ons avontuur begint, ligt $n rustig te slapen...", ""
DATA "*s2", "*c14", "<tik, tik, tik>", "*c3", "", "$n: 'huh?'"
DATA "&2=2"

DATA "1=0;4=1", "*c2"
DATA "Je probeert nog even verder te slapen.", ""
DATA "*c3", "$n: 'Gaaaaap..   ..zzzzzz'", ""
DATA "*s2", "*c14", "<tik, tik, tik>", "*c3", "", "$n: 'hmmmz?'"
DATA "&"

DATA "1=0;4=2", "*c3"
DATA "$n: 'Ja, ja, ik kom al!'", "", "*c2"
DATA "Met veel tegenzin klim je uit je bed."
DATA "Je strompelt naar de deur."
DATA "", "*c14", "<TIK, TIK, TIK!>", "*c3", ""
DATA "$n: 'Ik ben onderweg!'"
DATA "*c2", "", "Als je de deur van je kamer open doet, staat er niemand.", ""
DATA "*c3", "$n: 'Waar komt dat geluid dan vandaan?'"
DATA "&"

DATA "1=0;4=3", "*c2"
DATA "Je kijkt de hele kamer rond, op zoek naar de bron van het geluid"
DATA "", "*c3", "$n: 'Hey, er zit een raaf tegen het raam te tikken!'"
DATA "", "*c14", "<TIK, TIK, TIK!>",
DATA "&"

DATA "1=0;4=4", "*c2", "Je opent het raam. De raaf springt snel naar binnen."
DATA "Zo te zien heeft hij een brief aan zijn poot hangen."
DATA "&"

DATA "1=0;4=5;2=2", "*c2", "Je probeert de raaf weg te jagen.", "&"
DATA "1=0;4=6;2=2", "*c2", "Je probeert weer in bed te kruipen.", "&"
DATA "1=0;4=7;2=2", "*c2", "Je probeert het briefje van de raaf te pakken.", "&"
DATA "1=0;4>4;4<8;2=2", "*c2", "De raaf pikt naar je hand."
DATA "*c3", "$n: 'Grrr!'"
DATA "&"

DATA "1=0;4=12", "*c2", "Je geeft de raaf wat vogelvoer",
DATA "De raaf is je erg dankbaar!"
DATA "&23=4;4=8"

DATA "1=0;2=0", "*c2", "Je kijkt de kamer rond", "", "&"
DATA "1=0;2=0;20=0", "Je tas hangt aan de achterkant van de deur", "&"
DATA "1=0;2=0", "Je hebt een mooie kast tussen je bed en bureau in staan", "&"
DATA "1=0;2=0;23!4", "Een redelijk agressieve raaf zit op je bureau", "&"
DATA "1=0;2=0;23=4", "Een rustige raaf zit op je bureau", "&"

DATA "1=0;4=9;23!2;", "*c2", "De kast zit op slot", "&4=8"
DATA "1=0;4=9;23=2;", "*c2", "Je opent de kast met de sleutel", ""
DATA "*c3", "$n: 'Hey, hier ligt vogelvoer!'"
DATA "", "*c2", "Je pakt het vogelvoer uit de kast", "&4=8;23=3"
DATA "1=0;4=10;23=0", "*c2", "Je pakt je tas van achter de deur", "&20=1;22=100;23=1;4=8"
DATA "1=0;4=11;23!4", "*c2", "Je probeert het briefje van de raaf te pakken."
DATA "De raaf pikt naar je hand.", "*c3", "$n: 'Grrr!'"
DATA "&4=8"
DATA "1=0;4=11;23=4", "*c2", "Je probeert het briefje van de raaf te pakken."
DATA "De raaf blijft rustig.", "", "Je leest de brief:", "*c6", "*s1", ""
DATA "Beste $n,"
DATA "", "De koning is ernstig ziek."
DATA "Als beste koerier van het land smeken we je om ons te helpen"
DATA "", "Jij bent de enige die snel medicijnen naar ons kan brengen"
DATA "We hopen dat je dit bericht snel ontvangt en je snel de juiste medicijnen kan komen brengen"
DATA "", "Gegroet,", "  De dokter van de koning", "*s2", "*c2", ""
DATA "Je blijft een tijdje naar de brief staren.", "De koning?", "Heeft jouw hulp nodig?"
DATA "Je gaat snel de deur uit, richting een groot avontuur..."
DATA "&4=20;22=3;20=0;2=0"

' 5. Intro
DATA "1=5", "*c2"
DATA "$n wist snel aan de juiste medicijnen te komen,"
DATA "in de vorm van een magische drank."
DATA "$H is nu onderweg naar de koning, om ze te bezorgen."
DATA ""
DATA "Gelukkig is $n te paard onderweg om een magische drank te bezorgen!"
DATA "*s3", ""
DATA "*c4", "Auw! een laag hangende tak!", ""
DATA "*c2", "$n viel ineens hard van $z paard...",
DATA "*c3", "$n: 'Oh nee! De magische drank is stuk gevallen!'"
DATA "'En mijn paard is er vandoor!'"
DATA "*c2", "", "Daar stond $n dan.. Alleen in het bos, zonder vervoer of medicijnen..."
DATA "&"

' Travelling
DATA "1=1;3=2", "*c2", "Je wandelt naar het oosten, richting het bos", "", "&2=0;3=0"
DATA "1=1;3=4", "*c2", "Je wandelt naar het westen, richting het bos", "", "&2=0;3=0"
DATA "1=2;3=2", "*c2", "Je wandelt naar het oosten, richting de akkers", "", "&2=0;3=0"
DATA "1=2;3=1", "*c2", "Je wandelt naar het noorden, richting de akkers", "", "&2=0;3=0"
DATA "1=3;3=4", "*c2", "Je wandelt naar het westen, richting de heuvels", "", "&2=0;3=0"
DATA "1=3;3=1", "*c2", "Je klimt omhoog, richting de heuvels", "", "&2=0;3=0"
DATA "1=4;3=3", "*c2", "Je daalt af naar het zuiden, over een sompig pad, richting het moeras", "", "&2=0;3=0"
DATA "1=6;3=2", "*c2", "Je wandelt naar het zuiden, richting het dorp", "", "&2=0;3=0"
DATA "1=6;3=4", "*c2", "Je wandelt naar het westen, richting het dorp", "", "&2=0;3=0"
DATA "1=7;3=2", "*c2", "Je wandelt over een smal kronkelig pad, richting het donkere woud", "", "&2=0;3=0"

' Inventory
DATA "2=1", "*c15", "Je hebt het volgende bij je:", "&"
DATA "2=1;22>0", "*c7", "- #22 geldstukken", "&"
DATA "2=1;21=2", "*c7", "- een stok, opgeraapt in het bos", "&"
DATA "2=1;23>0;23<3", "*c7", "- de sleutel van je kast", "&23=2"
DATA "2=1;23=3", "*c7", "- vogelvoer", "&"

' 1. Bos
DATA "1=1;2=0", "*c2", "Je staat in het bos. Het is een stralende dag."
DATA "De wind laat de blaadjes ritselen.", ""
DATA "In het oosten zijn akkers", "In het westen zijn heuvels", "&"
DATA "1=1;2=0;20=0", "*c2", "Op de grond ligt je tas, en scherven van de fles medicijnen"
DATA "", "*c3", "$n: 'Verdorie, de medicijnen zijn echt verloren'"
DATA "*c2", "", "Je raapt de tas op", "&20=1;22=3"
DATA "1=1;2=0;21=0", "*c2", "Op de grond ligt een vers afgebroken tak"
DATA "&"
DATA "21=1;2=2", "*c2", "Je bukt en raapt de tak op. Je voelt nog even aan je hoofd."
DATA "Deze tak heeft je best pijn gedaan.", "&21=2"

' 2. Akkers
DATA "1=2;2=0", "*c2"
DATA "Je staat in een boeren landschap."
DATA "Aan de linkerkant van de weg staat een molen."
DATA "Aan de rechterkant van de weg staat een boerderij."
DATA "Er komt rook achter de boerderij vandaan.", ""
DATA "In het westen ligt een bos"
DATA "In het zuiden loopt een weg richting een dorp", "&"

' 3. Heuvels
DATA "1=3;2=0", "*c2"
DATA "Je staat in de heuvels. De zon schijnt heerlijk."
DATA "Er is een mooi uitzicht van de omgeving.", ""
DATA "Verder op het pad zie je een Mijn"
DATA "Naar het oosten loopt het pad naar het bos"
DATA "Naar het zuiden loopt een drassig pad een moeras in", "&"

' 4. Moeras
DATA "1=4;2=0", "*c2"
DATA "Je bent aan de rand van het moeras"
DATA "Aan het eind van het pad blijf je staan.", ""
DATA "*c3", "$n: 'Ik ga niet zomaar verder hier heen.'"
DATA " 'Ik wil eerst een goed plan hebben om aan medicijnen te komen'"
DATA " 'Wie weet wat hier allemaal tegen kan komen'"
DATA "*c2", "", "In de verte klinken rare bubbelende geluiden."
DATA "", "Er loopt een pad naar het noorden, omhoog richting heuvels."

DATA "*c3", "$n: 'Ik ga niet zomaar verder hier heen.'"
DATA " 'Ik wil eerst een goed plan hebben om aan medicijnen te komen'"
DATA " 'Wie weet wat hier allemaal tegen kan komen'"
DATA "*c2", "", "In de verte klinken rare bubbelende geluiden."
DATA "", "Er loopt een pad naar het noorden, omhoog richting heuvels.", "&"

' 6. Dorp
DATA "1=6;2=0", "*c2"
DATA "Je bent in het dorp", ""
DATA "Er hangt een gezellige sfeer", ""
DATA "Aan de linkerkant van de weg zit een bakker"
DATA "Aan de rechterkant van de weg een grote smederij", ""
DATA "De weg loopt verder naar het zuiden, richting een rivier"
DATA "Een klein verborgen pad lijd naar het oosten, richting een donker woud", "&"

' 7. Donkere woud
DATA "1=7;2=0", "*c2"
DATA "Je staat aan de rand van een donker woud."
DATA "Het ziet er erg donker en gevaarlijk uit", ""
DATA "*c3", "$n: 'Brr.. hier heb ik niets te zoeken'", "&"

DATA "END"

' Acties
2 DATA "1=0;4<2", "Word wakker", "4=2"
DATA "1=0;4<2", "Probeer nog even verder te slapen", "4=1"
DATA "1=0;4=2", "Kijk waar geluid vandaan komt", "4=3"
DATA "1=0;4=3", "Open raam", "4=4"
DATA "1=0;4>3;4<8", "Jaag de raaf weg", "4=5;5+1"
DATA "1=0;4>3;4<8", "Het tikken is weg! probeer weer in bed te kruipen", "4=6;5+1"
DATA "1=0;4>3;4<8", "Pak briefje van de raaf", "4=7;5+1"
DATA "1=0;4>3;4<8;5>2", "Kijk de kamer rond", "2=0;4=8"

DATA "2!0;1=0;4>7", "Bekijk omgeving", "2=0"
DATA "1=0;4=8", "Open de kast", "4=9;2=2"
DATA "1=0;4=8;20=0", "Pak je tas", "4=10;2=2"
DATA "1=0;4=8", "Pak briefje van de raaf", "4=11;2=2"
DATA "1=0;4=8;23=3", "Geef de raaf te eten", "4=12;2=2"
DATA "1=0;4=20", "Begin avontuur", "1=5;4=0"

DATA "1=5", "Bekijk omgeving", "1=1"
DATA "2!0;1!0", "Bekijk omgeving", "2=0"
DATA "20=1;2!1", "Bekijk inhoud van tas", "2=1"

' 1. Bos acties
DATA "1=1;21=0", "Raap de tak op", "21=1;2=2"
DATA "1=1", "Ga naar het oosten, richting de akkers", "1=2;3=2"
DATA "1=1", "Ga naar het westen, richting de heuvels", "1=3;3=4"

' 2. Akker acties
DATA "1=2", "Ga naar het westen, richting het bos", "1=1;3=4"
DATA "1=2", "Ga naar het zuiden, richting het dorp", "1=6;3=3"

' 3. Heuvel acties
DATA "1=3", "Ga naar het oosten, richting het bos", "1=1;3=2"
DATA "1=3", "Ga naar het zuiden, richting het moeras", "1=4;3=3"

' 4. Moeras acties
DATA "1=4", "Ga naar het noorden, richting de heuvels", "1=3;3=1"

' 6. Dorp Acties
DATA "1=6", "Ga naar het noorden, richting de akkers", "1=2;3=1"
DATA "1=6", "Ga naar het oosten, richting de het donkere woud", "1=7;3=2"

' 7. Donker woud acties
DATA "1=7", "Ga naar het westen, richting het dorp", "1=6;3=4"

DATA "END"

DIM SHARED Naam$
DIM SHARED Geslacht%
DIM SHARED Gebeurtenissen(100) AS INTEGER
DIM SHARED HuidigSpel%

CLS
LOCATE 5, 20
PRINT "Avontuur!"

AantalOpslag% = 0

FOR i% = 1 TO 5
	SpelOpslag$ = "OPSLAG" + STR$(i%) + ".SAV"
	OPEN SpelOpslag$ FOR INPUT AS 1
	IF errorflag = 0 THEN
		AantalOpslag% = AantalOpslag + 1
	END IF
	CLOSE
NEXT i%

LOCATE 7, 1
PRINT SPACE$(19); STRING$(40, "-")
PRINT SPACE$(19); "1 ) Nieuw spel"
IF AantalOpslag% > 0 THEN
	FOR i% = 2 TO AantalOpslag% + 1
		PRINT SPACE$(18); i%; ") Spel"; (i% - 1); "laden"
	NEXT i%
END IF
PRINT SPACE$(19); STRING$(40, "-")
PRINT SPACE$(19); "Toetsen tijdens spelen:"
PRINT SPACE$(19); "S) Spel opslaan"
PRINT SPACE$(19); "esc) Stoppen"
PRINT SPACE$(19); STRING$(40, "-")

DO
	DO
		Keuze$ = INKEY$
	LOOP UNTIL Keuze$ <> ""
	Gekozen% = ASC(Keuze$) - ASC("1")
LOOP UNTIL Gekozen% >= 0 AND Gekozen% <= AantalOpslag%

IF Gekozen% = 0 THEN GOTO Start

HuidigSpel% = Gekozen%

SpelOpslag$ = "OPSLAG" + STR$(HuidigSpel%) + ".SAV"
OPEN SpelOpslag$ FOR INPUT AS 1
LINE INPUT #1, Naam$
LINE INPUT #1, BestandRegel$
Geslacht% = VAL(BestandRegel$)
FOR i% = 0 TO 100
	LINE INPUT #1, BestandRegel$
	Gebeurtenissen(i%) = VAL(BestandRegel$)
NEXT i%
CLOSE 1

GOTO SpelLus

Start:
HuidigSpel% = AantalOpslag% + 1
CLS
PRINT "Welkom!"
PRINT
INPUT "Hoi, wat is jouw naam"; Naam$
INPUT "Ben je een jongen of een meisje? (j/m) ", Invoer$
IF UCASE$(LEFT$(Invoer$, 1)) = "J" THEN
	Geslacht% = 1
ELSE
	Geslacht% = 0
END IF

SpelLus:
KEY(16) ON
DO
	CLS
	CALL ToonGebeurtenis
	CALL ToonActies
LOOP UNTIL Gebeurtenissen(0) <> 0

END

Stoppen:
END

Foutafhandeling:
	fout = ERR
RESUME NEXT

Opslaan:
	SpelOpslag$ = "OPSLAG" + STR$(HuidigSpel%) + ".SAV"
  OPEN SpelOpslag$ FOR OUTPUT AS 1
	PRINT #1, Naam$
	PRINT #1, Geslacht%
	FOR i% = 0 TO 100
		PRINT #1, Gebeurtenissen(i%)
	NEXT i%
	CLOSE 1
RETURN

FUNCTION Interpoleer$ (zin$)
	result$ = ""

	FOR i% = 1 TO LEN(zin$)
		char$ = MID$(zin$, i%, 1)
		IF char$ = "$" THEN
			SELECT CASE MID$(zin$, i%, 2)
				CASE "$n"
					result$ = result$ + Naam$
				CASE "$h"
					IF Geslacht% THEN
						result$ = result$ + "hij"
					ELSE
						result$ = result$ + "zij"
					END IF
				CASE "$H"
					IF Geslacht% THEN
						result$ = result$ + "Hij"
					ELSE
						result$ = result$ + "Zij"
					END IF
				CASE "$z"
					IF Geslacht% THEN
						result$ = result$ + "zijn"
					ELSE
						result$ = result$ + "haar"
					END IF
				CASE "$Z"
					IF Geslacht% THEN
						result$ = result$ + "Zijn"
					ELSE
						result$ = result$ + "Haar"
					END IF
			END SELECT
			i% = i% + 1
		ELSEIF char$ = "#" THEN
			slot% = VAL(MID$(zin$, i% + 1, 2))
			result$ = result$ + STR$(Gebeurtenissen(slot%))
			i% = i% + 2
		ELSE
			result$ = result$ + char$
		END IF
	NEXT i%

	Interpoleer$ = result$
END FUNCTION

SUB Muteer (slot%, op$, value%)
	'PRINT "Muteer"; slot%; Gebeurtenissen(slot%); op$; value%
	SELECT CASE op$
		CASE "="
			Gebeurtenissen(slot%) = value%
		CASE "+"
			Gebeurtenissen(slot%) = Gebeurtenissen(slot%) + value%
	END SELECT
END SUB

SUB Tekst (Kleur%, Regel$)
	COLOR Kleur%
	IF Kleur% <> 4 THEN
		TIMER ON
		Overslaan = 0
		FOR i% = 1 TO LEN(Regel$)
			TijdStart = TIMER
			PRINT MID$(Regel$, i%, 1);
			IF Overslaan = 0 THEN
				DO
					IF INKEY$ = " " THEN
						Overslaan = -1
					END IF
				LOOP UNTIL TIMER > TijdStart + .05
			END IF
		NEXT i%
		TIMER OFF
		PRINT
	ELSE
		PRINT Regel$
		TIMER ON
		TijdStart = TIMER
		DO
		LOOP UNTIL TIMER > TijdStart + (.05 * LEN(Regel$))
		TIMER OFF
	END IF
	COLOR 7
END SUB

FUNCTION Toegestaan% (predicate$)
	IF predicate$ = "END" THEN
		Toegestaan = 0
		EXIT FUNCTION
	END IF

	Toegestaan = 1

	slot$ = ""
	op$ = ""
	value$ = ""

	FOR i% = 1 TO LEN(predicate$)
		char$ = MID$(predicate$, i%, 1)
		SELECT CASE char$
			CASE "0" TO "9"
				IF op$ = "" THEN
					slot$ = slot$ + char$
				ELSE
					value$ = value$ + char$
				END IF
			CASE "=", "!", ">", "<"
				op$ = char$
			CASE ";"
				resultaat = Toets(VAL(slot$), op$, VAL(value$))
				IF resultaat = 0 THEN
					Toegestaan = 0
					EXIT FUNCTION
				END IF
				op$ = ""
				value$ = ""
				slot$ = ""
		END SELECT

	NEXT i%

	IF op$ <> "" THEN
		Toegestaan = Toets(VAL(slot$), op$, VAL(value$))
	END IF

END FUNCTION

FUNCTION Toets (slot%, op$, value%)
	'PRINT "Toets"; slot%, Gebeurtenissen(slot%), op$; value%
	IF op$ = "=" AND Gebeurtenissen(slot%) = value% THEN
		Toets = 1
	ELSEIF op$ = "!" AND Gebeurtenissen(slot%) <> value% THEN
		Toets = 1
	ELSEIF op$ = ">" AND Gebeurtenissen(slot%) > value% THEN
		Toets = 1
	ELSEIF op$ = "<" AND Gebeurtenissen(slot%) < value% THEN
		Toets = 1
	ELSE
		Toets = 0
	END IF

END FUNCTION

SUB ToonActies
	DIM actie(10) AS STRING
	DIM ActieTekst(10) AS STRING
	ActieTeller% = 0

	RESTORE 2
	DO
		READ predicate$
		IF Toegestaan%(predicate$) THEN
			READ ActieTekst(ActieTeller%), actie(ActieTeller%)
			ActieTeller% = ActieTeller% + 1
		ELSEIF predicate$ <> "END" THEN
			READ zin$, change$
		END IF
	LOOP UNTIL predicate$ = "END"

	COLOR 15
	PRINT
	PRINT "Wat ga je doen:"
	COLOR 7
	FOR i% = 1 TO ActieTeller%
		PRINT i%; ") "; ActieTekst(i% - 1)
	NEXT i%

	DO
		DO
			Keuze$ = INKEY$
		LOOP UNTIL Keuze$ <> ""
		Gekozen% = ASC(Keuze$) - ASC("1")
	LOOP UNTIL Gekozen% >= 0 AND Gekozen% < ActieTeller%

	VoerActieUit (actie(Gekozen%))

END SUB

SUB ToonGebeurtenis
	RESTORE 1
	verteller% = 2
	DO
		READ predicate$
		IF Toegestaan%(predicate$) THEN
			DO
				READ zin$
				op$ = LEFT$(zin$, 1)
				SELECT CASE op$
					CASE "&"
						' Einde en acties
						acties$ = MID$(zin$, 2, LEN(zin$) - 1)
						IF acties$ <> "" THEN
							CALL VoerActieUit(acties$)
						END IF
					CASE "*"
						' Markup instructie
						commando$ = MID$(zin$, 2, 1)
						gegevens$ = MID$(zin$, 3, LEN(zin$) - 2)
						SELECT CASE commando$
							CASE "c"
								verteller% = VAL(gegevens$)
							CASE "s"
								SLEEP VAL(gegevens$)
							CASE "p"
								PLAY gegevens$
						END SELECT
					CASE ELSE
						CALL Tekst(verteller%, Interpoleer(zin$))
				END SELECT
			LOOP UNTIL op$ = "&"
		ELSEIF predicate$ <> "END" THEN
			DO
				READ zin$
			LOOP UNTIL LEFT$(zin$, 1) = "&"
		END IF
	LOOP UNTIL predicate$ = "END"

END SUB

SUB VoerActieUit (actie$)
	slot$ = ""
	op$ = ""
	value$ = ""
	FOR i% = 1 TO LEN(actie$) + 1
		char$ = MID$(actie$, i%, 1)
		SELECT CASE char$
			CASE "0" TO "9"
				IF op$ = "" THEN
					slot$ = slot$ + char$
				ELSE
					value$ = value$ + char$
				END IF
			CASE "=", "+"
				op$ = char$
			CASE ";"
				CALL Muteer(VAL(slot$), op$, VAL(value$))
				op$ = ""
				value$ = ""
				slot$ = ""
		END SELECT

	NEXT i%

	IF op$ <> "" THEN
		CALL Muteer(VAL(slot$), op$, VAL(value$))
	END IF

END SUB
