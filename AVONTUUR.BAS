DECLARE SUB Muteer (slot%, op$, value%)
DECLARE SUB VoerActieUit (actie$)
DECLARE SUB ToonActies ()
DECLARE FUNCTION Toets! (slot%, op$, value%)
DECLARE FUNCTION Toegestaan% (predicate$)
DECLARE SUB ToonGebeurtenis ()
DECLARE SUB Tekst (Kleur%, Regel$)
DECLARE FUNCTION Interpoleer$ (Tekst$)
KEY 15, CHR$(0) + CHR$(1)
KEY(15) ON
ON KEY(15) GOSUB Stoppen
' Gebeurtenissen
' 0 = Spel gewonnen (1), Bezig(0), Verloren(2)
' 1 = location
'   0 = Intro
' 2 = open inventory

' 20 = hasBag
' 21 = hasBranch
' 22 = money

' Intro
1 DATA "1=0",
DATA "*c2", "Er was eens, lang geleden een stoere koerier..."
DATA "", "$Z naam was $n", ""
DATA "$H was altijd super snel in het bezorgen van brieven en pakketjes."
DATA "Dingen bezorgen was dan ook $z lust en $z leven."
DATA ""
DATA "Op een dag is $h onderweg met een heel belangrijk pakket voor de koning..."
DATA "De koning is ernstig ziek, en heeft dringend medicijnen nodig!"
DATA ""
DATA "Gelukkig is $n te paard onderweg om een magische drank te bezorgen!"
DATA "*s3", ""
DATA "*c4", "Auw! een laag hangende tak!", ""
DATA "*c2", "$n viel ineens hard van $z paard...",
DATA "*c3", "$n: 'Oh nee! De magische drank is stuk gevallen!'"
DATA "'En mijn paard is er vandoor!'"
DATA "*c2", "", "Daar stond $n dan.. Alleen in het bos, zonder vervoer of medicijnen..."
DATA "&"

' Inventory
DATA "2=1", "*c15", "Je hebt het volgende bij je:", "&"
DATA "2=1;22>0", "*c7", "- #22 ducaten", "&"
DATA "2=1;21=1", "*c7", "- een stok, opgeraapt in het bos", "&"

' Bos
DATA "1=1;2=0", "*c2", "Je staat in het bos. Het is een stralende dag."
DATA "De wind laat de blaadjes ritselen.", ""
DATA "In het oosten zijn akkers", "In het westen zijn heuvels", "&"
DATA "1=1;2=0;20=0", "*c2", "Op de grond ligt je tas, en scherven van de fles medicijen"
DATA "", "*c3", "$n: 'Verdorie, de medicijnen zijn echt verloren'"
DATA "*c2", "", "Je raapt de tas op", "&20=1;21=3"
DATA "1=1;2=0;21=0", "*c2", "Op de grond ligt een vers afgebroken tak"
DATA "&"

DATA "END"

' Acties
2 DATA "1=0", "Bekijk omgeving", "1=1"
DATA "2!0", "Bekijk omgeving", "2=0"
DATA "20=1;2!1", "Bekijk inhoud van tas", "2=1"
DATA "END"

DIM SHARED Naam$
DIM SHARED Geslacht%
DIM SHARED Gebeurtenissen(100) AS INTEGER

CLS
INPUT "Hoi, wat is jouw naam"; Naam$
INPUT "Ben je een jongen of een meisje? (j/m) ", Invoer$
IF UCASE$(LEFT$(Invoer$, 1)) = "J" THEN
  Geslacht% = 1
ELSE
  Geslacht% = 0
END IF

DO
  CLS
  CALL ToonGebeurtenis
  CALL ToonActies
LOOP UNTIL Gebeurtenissen(0) <> 0

END

Stoppen:
END


'SUB BekijkTas
  'CLS
  'COLOR 15
  'PRINT "Je hebt het volgende bij je:"
  'COLOR 7
  'CALL Tekst(7, "-" + STR$(money%) + " dukaten")
  'IF inventory(0) = 1 THEN
    'CALL Tekst(7, "- een stok, opgeraapt in het bos")
  'END IF
  'PRINT
'END SUB

'SUB PlaatsAkkers
  'PRINT
  'Gekeken = 1

  'DO
    'CALL wisActies
    'i% = 0
    'IF Gekeken = 0 THEN
      'acties(i%) = "Kijk rond"
      'actieTeksten$(i%) = "Bekijk omgeving"
      'i% = i% + 1
    'ELSE
      'CALL Tekst(2, "Je staat in een boeren landschap.")
      'CALL Tekst(2, "Aan de linkerkant van de weg staat een molen.")
      'CALL Tekst(2, "Aan de rechterkant van de weg staat een boerderij.")
      'CALL Tekst(2, "Er komt rook achter de boerderij vandaan.")
      'PRINT
      'CALL Tekst(2, "In het westen ligt een bos")
      'CALL Tekst(2, "In het zuiden loopt een weg richting een dorp")
    'END IF

    'acties(i%) = "Bekijk tas"
    'actieTeksten$(i%) = "Bekijk inhoud van tas"

    'i% = i% + 1
    'acties$(i%) = "Westen"
    'actieTeksten$(i%) = "Ga naar het westen, richting het bos"

    'Opdracht$ = acties$(welkeActie%)

    'SELECT CASE Opdracht$
    'CASE "Westen"
      'CLS
      'CALL Tekst(2, "Je wandelt naar het westen, richting het bos.")
      'Plaats$ = "Bos"
    'CASE "Bekijk tas"
      'CALL BekijkTas
      'Gekeken = 0
    'CASE "Kijk rond"
      'Gekeken = 1
      'CLS
    'END SELECT

  'LOOP UNTIL Plaats$ <> "Akkers"

'END SUB

'SUB PlaatsBos
  'PRINT
  'Gekeken = 1

  'DO
    'CALL wisActies
    'i% = 0
    'IF Gekeken = 0 THEN
      'acties(i%) = "Kijk rond"
      'actieTeksten$(i%) = "Bekijk omgeving"
      'i% = i% + 1
    'ELSE
      'CALL Tekst(2, "Je staat in het bos. Het is een stralende dag.")
      'CALL Tekst(2, "De wind laat de blaadjes ritselen.")
      'PRINT
      'CALL Tekst(2, "In het oosten zijn akkers")
      'CALL Tekst(2, "In het westen zijn heuvels")
      'IF inventory(0) = 0 THEN
        'CALL Tekst(2, "Op de grond ligt een vers afgebroken tak")
      'END IF
    'END IF

    'acties(i%) = "Bekijk tas"
    'actieTeksten$(i%) = "Bekijk inhoud van tas"

    'IF inventory(0) = 0 THEN
      'i% = i% + 1
      'acties$(i%) = "Pak tak"
      'actieTeksten$(i%) = "Raap de tak op"
    'END IF

    'i% = i% + 1
    'acties$(i%) = "Oosten"
    'actieTeksten$(i%) = "Ga naar het oosten, richting de akkers"
    'i% = i% + 1
    'acties$(i%) = "Westen"
    'actieTeksten$(i%) = "Ga naar het westen, richting de heuvels"

    'Opdracht$ = acties$(welkeActie%)

    'SELECT CASE Opdracht$
    'CASE "Oosten"
      'CLS
      'CALL Tekst(2, "Je wandelt naar het oosten, richting de akkers.")
      'Plaats$ = "Akkers"
    'CASE "Westen"
      'CLS
      'CALL Tekst(2, "Je wandelt naar het westen, richting de heuvels.")
      'Plaats$ = "Heuvels"
    'CASE "Pak tak"
      'CLS
      'CALL Tekst(2, "Je bukt en raapt de tak op. Je voelt nog even aan je hoofd.")
      'CALL Tekst(2, "Deze tak heeft je best pijn gedaan.")
      'inventory(0) = 1
      'Gekeken = 0
    'CASE "Bekijk tas"
      'CALL BekijkTas
      'Gekeken = 0
    'CASE "Kijk rond"
      'Gekeken = 1
      'CLS
    'END SELECT
  'LOOP UNTIL Plaats$ <> "Bos"

'END SUB

'SUB PlaatsHeuvels
  'PRINT
  'Gekeken = 1

  'DO
    'CALL wisActies
    'i% = 0
    'IF Gekeken = 0 THEN
      'acties(i%) = "Kijk rond"
      'actieTeksten$(i%) = "Bekijk omgeving"
      'i% = i% + 1
    'ELSE
      'CALL Tekst(2, "Je staat in de heuvels. De zon schijnt heerlijk.")
      'CALL Tekst(2, "Er is een mooi uitzicht van de omgeving.")
      'PRINT
      'CALL Tekst(2, "Verder op het pad zie je een Mijn")
      'CALL Tekst(2, "Naar het oosten loopt het pad naar het bos")
    'END IF

    'acties(i%) = "Bekijk tas"
    'actieTeksten$(i%) = "Bekijk inhoud van tas"

    'i% = i% + 1
    'acties$(i%) = "Oosten"
    'actieTeksten$(i%) = "Ga naar het oosten, richting het bos"

    'Opdracht$ = acties$(welkeActie%)

    'SELECT CASE Opdracht$
    'CASE "Oosten"
      'CLS
      'CALL Tekst(2, "Je wandelt naar het oosten, richting het bos.")
      'Plaats$ = "Bos"
    'CASE "Bekijk tas"
      'CALL BekijkTas
      'Gekeken = 0
    'CASE "Kijk rond"
      'Gekeken = 1
      'CLS
    'END SELECT
  'LOOP UNTIL Plaats$ <> "Heuvels"

'END SUB

'SUB Start
  'CLS
  'INPUT "Hoi, wat is jouw naam"; Naam$
  'INPUT "Ben je een jongen of een meisje? (j/m) ", gender$
  'IF UCASE$(LEFT$(gender$, 1)) = "J" THEN
    'Hij$ = "hij"
    'Zijn$ = "zijn"
  'ELSE
    'Hij$ = "zij"
    'Zijn$ = "haar"
  'END IF
  'money% = 3

  'CLS
  'CALL Tekst(2, "Er was eens, lang geleden een stoere koerier...")
  'PRINT
  'CALL Tekst(2, Zijn$ + " naam was " + Naam$ + ".")
  'PRINT
  'CALL Tekst(2, Hij$ + " was altijd super snel in het bezorgen van brieven en pakketjes.")
  'CALL Tekst(2, "Dingen bezorgen was dan ook " + Zijn$ + " lust en " + Zijn$ + " leven.")
  'PRINT
  'CALL Tekst(2, "Op een dag is " + Hij$ + " onderweg met een heel belangrijk pakket voor de koning...")
  'CALL Tekst(2, "De koning is ernstig ziek, en heeft dringend medicijnen nodig!")
  'PRINT
  'CALL Tekst(2, "Gelukkig is " + Naam$ + " te paard onderweg om een magische drank te bezorgen!")

  'SLEEP 3
  'PRINT
  'CALL Tekst(4, "Auw! een laag hangende tak!")
  'PRINT

  'CALL Tekst(2, Naam$ + " viel ineens hard van " + Zijn$ + " paard...")
  'PRINT
  'CALL Tekst(3, Naam$ + ": 'Oh nee! De magische drank is stuk gevallen!'")
  'CALL Tekst(3, "'En mijn paard is er vandoor!'")
  'PRINT
  'CALL Tekst(2, "Daar stond " + Naam$ + " dan.. Alleen in het bos, zonder vervoer of medicijnen...")

  'Gekeken = 0

  'DO
    'CALL wisActies
    'i% = 0

    'IF Gekeken <> 1 THEN
      'acties(i%) = "Kijk rond"
      'actieTeksten$(i%) = "Bekijk omgeving"
      'i% = i% + 1
    'END IF

    'IF Gekeken <> 0 THEN
      'acties(i%) = "Bekijk tas"
      'actieTeksten$(i%) = "Bekijk inhoud van tas"

      'IF inventory(0) = 0 THEN
        'i% = i% + 1
        'acties$(i%) = "Pak tak"
        'actieTeksten$(i%) = "Raap de tak op"
      'END IF

      'i% = i% + 1
      'acties$(i%) = "Oosten"
      'actieTeksten$(i%) = "Ga naar het oosten, richting de akkers"
      'i% = i% + 1
      'acties$(i%) = "Westen"
      'actieTeksten$(i%) = "Ga naar het westen, richting de heuvels"
    'END IF

    'Opdracht$ = acties$(welkeActie%)

    'SELECT CASE Opdracht$
    'CASE "Kijk rond"
      'CLS
      'CALL Tekst(2, "Je staat in het bos. Het is een stralende dag.")
      'CALL Tekst(2, "De wind laat de blaadjes ritselen.")
      'PRINT
      'CALL Tekst(2, "In het oosten zijn akkers")
      'CALL Tekst(2, "In het westen zijn heuvels")
      'IF Gekeken = 0 THEN
        'CALL Tekst(2, "Op de grond ligt je tas, en scherven van de fles medicijen")
        'PRINT
        'CALL Tekst(3, Naam$ + ": 'Verdorie, de medicijnen zijn echt verloren'")
        'PRINT
        'CALL Tekst(2, "Je raapt de tas op")
      'END IF
      'IF inventory(0) = 0 THEN
        'CALL Tekst(2, "Op de grond ligt een vers afgebroken tak")
      'END IF
      'Gekeken = 1
    'CASE "Oosten"
      'CLS
      'CALL Tekst(2, "Je wandeld naar het oosten, richting de akkers.")
      'Plaats$ = "Akkers"
    'CASE "Westen"
      'CLS
      'CALL Tekst(2, "Je wandeld naar het westen, richting de naar de heuvels.")
      'Plaats$ = "Heuvels"
    'CASE "Pak tak"
      'CLS
      'CALL Tekst(2, "Je bukt en raapt de tak op. Je voelt nog even aan je hoofd.")
      'CALL Tekst(2, "Deze tak heeft je best pijn gedaan.")
      'inventory(0) = 1
      'Gekeken = 2
    'CASE "Bekijk tas"
      'CALL BekijkTas
      'Gekeken = 2
    'END SELECT

  'LOOP UNTIL Plaats$ <> "Start"

'END SUB

'FUNCTION welkeActie%
  'PRINT
  'COLOR 15
  'PRINT "Wat ga je doen?"
  'COLOR 7

  'maxActies% = 0
  'FOR i% = LBOUND(acties$) TO UBOUND(acties$)
    'IF acties$(i%) <> "" THEN
      'PRINT (i% + 1); ") "; actieTeksten$(i%)
      'maxActies% = maxActies% + 1
    'END IF
  'NEXT i%

  'DO
    'DO
      'Keuze$ = INKEY$
    'LOOP UNTIL Keuze$ <> ""
    'Gekozen% = ASC(Keuze$) - ASC("1")
  'LOOP UNTIL Gekozen% >= 0 AND Gekozen% < maxActies%

  'welkeActie = Gekozen%

'END FUNCTION

'SUB wisActies
  'FOR i% = LBOUND(acties$) TO UBOUND(acties$)
    'acties$(i%) = ""
    'actieTeksten$(i%) = ""
  'NEXT i%
'END SUB

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
  SELECT CASE op$
    CASE "="
      Gebeurtenissen(slot%) = value%
    CASE "+"
      Gebeurtenissen(slot%) = Gebeurtenissen(slot%) + value%
  END SELECT
END SUB

SUB Tekst (Kleur%, Regel$)
  IF Kleur% = -1 THEN
    SLEEP VAL(Regel$)
    EXIT SUB
  END IF
  IF Kleur% = -2 THEN
    PLAY Regel$
    EXIT SUB
  END IF

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
            acties$ = MID$(zin$, 1, LEN(zin$) - 1)
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

  FOR i% = 1 TO LEN(actie$)
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

