DECLARE SUB PlaatsBos ()
DECLARE SUB PlaatsAkkers ()
DECLARE SUB BekijkTas ()
DECLARE SUB wisActies ()
DECLARE FUNCTION welkeActie% ()
DECLARE SUB tekst (Kleur%, Regel$)
DECLARE SUB Start ()
DECLARE SUB forest ()

KEY 15, CHR$(0) + CHR$(1)
KEY(15) ON
ON KEY(15) GOSUB Stoppen

DIM SHARED Plaats$
DIM SHARED Naam$
DIM SHARED Hij$
DIM SHARED Zijn$
DIM SHARED acties(8) AS STRING
DIM SHARED actieTeksten(8) AS STRING
DIM SHARED money%
DIM SHARED inventory(1) AS INTEGER
' waarde: 0 = niet in bezit, 1 = in bezit, 2 = gebruikt
' lokatie in lijst en betekenis
' 0 = Stok

Plaats$ = "Start"

DO
SELECT CASE Plaats$
CASE "Start"
  CALL Start
CASE "Akkers"
  CALL PlaatsAkkers
CASE "Bos"
  CALL PlaatsBos
CASE ELSE
  END
END SELECT
LOOP
END
Stoppen:
END

SUB BekijkTas
  CLS
  COLOR 15
  PRINT "Je hebt het volgende bij je:"
  COLOR 7
  CALL tekst(7, "-" + STR$(money%) + " dukaten")
  IF inventory(0) = 1 THEN
    CALL tekst(7, "- een stok, opgeraapt in het bos")
  END IF
  PRINT
END SUB

SUB PlaatsAkkers
  PRINT
  Gekeken = 1

  DO
    CALL wisActies
    i% = 0
    IF Gekeken = 0 THEN
      acties(i%) = "Kijk rond"
      actieTeksten$(i%) = "Bekijk omgeving"
      i% = i% + 1
    ELSE
      CALL tekst(2, "Je staat in een boeren landschap.")
      CALL tekst(2, "Aan de linkerkant van de weg staat een molen.")
      CALL tekst(2, "Aan de rechterkant van de weg staat een boerderij.")
      CALL tekst(2, "Er komt rook achter de boerderij vandaan.")
      PRINT
      CALL tekst(2, "In het westen westen ligt een bos")
      CALL tekst(2, "In het zuiden loop een weg richting een dorp")
    END IF

    acties(i%) = "Bekijk tas"
    actieTeksten$(i%) = "Bekijk inhoud van tas"

    i% = i% + 1
    acties$(i%) = "Westen"
    actieTeksten$(i%) = "Ga naar het westen, richting het bos"

    Opdracht$ = acties$(welkeActie%)

    SELECT CASE Opdracht$
    CASE "Westen"
      CLS
      CALL tekst(2, "Je wandeld naar het westen, richting het bos.")
      Plaats$ = "Bos"
    CASE "Bekijk tas"
      CALL BekijkTas
      Gekeken = 0
    CASE "Kijk rond"
      Gekeken = 1
      CLS
    END SELECT

  LOOP UNTIL Plaats$ <> "Akkers"

END SUB

SUB PlaatsBos
  PRINT
  Gekeken = 1

  DO
    CALL wisActies
    i% = 0
    IF Gekeken = 0 THEN
      acties(i%) = "Kijk rond"
      actieTeksten$(i%) = "Bekijk omgeving"
      i% = i% + 1
    ELSE
      CALL tekst(2, "Je staat in het bos. Het is een stralende dag.")
      CALL tekst(2, "De wind laat de blaadjes ritselen.")
      PRINT
      CALL tekst(2, "In het oosten zijn akkers")
      CALL tekst(2, "In het westen zijn heuvels")
      IF inventory(0) = 0 THEN
        CALL tekst(2, "Op de grond ligt een vers afgebroken tak")
      END IF
    END IF

    acties(i%) = "Bekijk tas"
    actieTeksten$(i%) = "Bekijk inhoud van tas"

    IF inventory(0) = 0 THEN
      i% = i% + 1
      acties$(i%) = "Pak tak"
      actieTeksten$(i%) = "Raap de tak op"
    END IF

    i% = i% + 1
    acties$(i%) = "Oosten"
    actieTeksten$(i%) = "Ga naar het oosten, richting de akkers"
    i% = i% + 1
    acties$(i%) = "Westen"
    actieTeksten$(i%) = "Ga naar het westen, richting de heuvels"

    Opdracht$ = acties$(welkeActie%)

    SELECT CASE Opdracht$
    CASE "Oosten"
      CLS
      CALL tekst(2, "Je wandeld naar het oosten, richting de akkers.")
      Plaats$ = "Akkers"
    CASE "Westen"
      CLS
      CALL tekst(2, "Je wandeld naar het westen, richting de naar de heuvels.")
      Plaats$ = "Heuvels"
    CASE "Pak tak"
      CLS
      CALL tekst(2, "Je bukt en raapt de tak op. Je voelt nog even aan je hoofd.")
      CALL tekst(2, "Deze tak heeft je best pijn gedaan.")
      inventory(0) = 1
      Gekeken = 0
    CASE "Bekijk tas"
      CALL BekijkTas
      Gekeken = 0
    CASE "Kijk rond"
      Gekeken = 1
      CLS
    END SELECT
  LOOP UNTIL Plaats$ <> "Bos"

END SUB

SUB Start
  CLS
  INPUT "Hoi, wat is jouw naam"; Naam$
  INPUT "Ben je een jongen of een meisje? (j/m) ", gender$
  IF UCASE$(LEFT$(gender$, 1)) = "J" THEN
    Hij$ = "hij"
    Zijn$ = "zijn"
  ELSE
    Hij$ = "zij"
    Zijn$ = "haar"
  END IF
  money% = 3

  CLS
  CALL tekst(2, "Er was eens, lang geleden een stoere koerier.")
  CALL tekst(2, Hij$ + " was altijd super snel in het bezorgen van brieven en pakketjes.")
  CALL tekst(2, "Dingen bezorgen was dan ook " + Zijn$ + " lust en " + Zijn$ + " leven.")
  PRINT
  CALL tekst(2, "Op een dag was " + Hij$ + " onderweg met een heel belangrijke pakket voor de koning...")
  CALL tekst(2, "De koning was ernstig ziek, en had dringend medicijnen nodig!")
  PRINT
  CALL tekst(2, "Gelukkig is " + Naam$ + " te paard onderweg om een magische drank te bezorgen!")

  SLEEP 3
  PRINT
  CALL tekst(4, "Auw! een laag hangende tak!")
  PRINT

  CALL tekst(2, Naam$ + " viel ineens hard van " + Zijn$ + " paard...")
  PRINT
  CALL tekst(3, Naam$ + ": 'Oh nee! De magische drank is stuk gevallen!'")
  CALL tekst(3, "'En mijn paard is er vandoor!'")
  PRINT
  CALL tekst(2, "Daar stond " + Naam$ + " dan.. Alleen in het bos, zonder vervoer of medicijnen...")

  Gekeken = 0

  DO
    CALL wisActies
    i% = 0

    IF Gekeken <> 1 THEN
      acties(i%) = "Kijk rond"
      actieTeksten$(i%) = "Bekijk omgeving"
      i% = i% + 1
    END IF

    IF Gekeken <> 0 THEN
      acties(i%) = "Bekijk tas"
      actieTeksten$(i%) = "Bekijk inhoud van tas"

      IF inventory(0) = 0 THEN
        i% = i% + 1
        acties$(i%) = "Pak tak"
        actieTeksten$(i%) = "Raap de tak op"
      END IF

      i% = i% + 1
      acties$(i%) = "Oosten"
      actieTeksten$(i%) = "Ga naar het oosten, richting de akkers"
      i% = i% + 1
      acties$(i%) = "Westen"
      actieTeksten$(i%) = "Ga naar het westen, richting de heuvels"
    END IF

    Opdracht$ = acties$(welkeActie%)

    SELECT CASE Opdracht$
    CASE "Kijk rond"
      CLS
      CALL tekst(2, "Je staat in het bos. Het is een stralende dag.")
      CALL tekst(2, "De wind laat de blaadjes ritselen.")
      PRINT
      CALL tekst(2, "In het oosten zijn akkers")
      CALL tekst(2, "In het westen zijn heuvels")
      IF Gekeken = 0 THEN
        CALL tekst(2, "Op de grond ligt je tas, en scherven van de fles medicijen")
        CALL tekst(3, Naam$ + ": 'Verdorie, de medicijnen zijn echt verloren'")
        PRINT
        CALL tekst(2, "Je raap de tas op")
      END IF
      IF inventory(0) = 0 THEN
        CALL tekst(2, "Op de grond ligt een vers afgebroken tak")
      END IF
      Gekeken = 1
    CASE "Oosten"
      CLS
      CALL tekst(2, "Je wandeld naar het oosten, richting de akkers.")
      Plaats$ = "Akkers"
    CASE "Westen"
      CLS
      CALL tekst(2, "Je wandeld naar het westen, richting de naar de heuvels.")
      Plaats$ = "Heuvels"
    CASE "Pak tak"
      CLS
      CALL tekst(2, "Je bukt en raapt de tak op. Je voelt nog even aan je hoofd.")
      CALL tekst(2, "Deze tak heeft je best pijn gedaan.")
      inventory(0) = 1
      Gekeken = 2
    CASE "Bekijk tas"
      CALL BekijkTas
      Gekeken = 2
    END SELECT

  LOOP UNTIL Plaats$ <> "Start"

END SUB

SUB tekst (Kleur%, Regel$)
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

FUNCTION welkeActie%
  PRINT
  COLOR 15
  PRINT "Wat ga je doen?"
  COLOR 7

  maxActies% = 0
  FOR i% = LBOUND(acties$) TO UBOUND(acties$)
    IF acties$(i%) <> "" THEN
      PRINT (i% + 1); ") "; actieTeksten$(i%)
      maxActies% = maxActies% + 1
    END IF
  NEXT i%

  DO
    DO
      Keuze$ = INKEY$
    LOOP UNTIL Keuze$ <> ""
    Gekozen% = ASC(Keuze$) - ASC("1")
  LOOP UNTIL Gekozen% >= 0 AND Gekozen% < maxActies%

  welkeActie = Gekozen%

END FUNCTION

SUB wisActies
  FOR i% = LBOUND(acties$) TO UBOUND(acties$)
    acties$(i%) = ""
    actieTeksten$(i%) = ""
  NEXT i%
END SUB

