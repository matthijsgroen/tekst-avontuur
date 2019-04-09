# Data formaat voor 'Avontuur'

Het hele spel heeft 3 variabelen:

1. Naam van de speler
2. Geslacht van de speler
3. Lijst van 100 getallen, voor de speltoestand

In het begin van het spel zijn deze allemaal ingevuld, waarbij de spel toestand
bestaat uit 100 nullen ('0')

## De spel lus

De inhoud van het spel data alles zal laten werken, bestaat uit 2 lijsten van
strings en het laatste elemen moet `"END"` bevatten. (Dit omdat QBasic niet weet
hoe lang de data lijst is)

1. De tekst voor het scherm te omschrijven
2. De acties die de speler uit kan voeren

Deze lijsten worden per handeling van boven naar beneden verwerkt:

Eerst de lijst om het scherm te omschrijven, daarna de lijst met acties

Vervolgens als de speler een actie kiest, begint het weer van voor af aan,
totdat het spel afgesloten wordt, of het eerste getal van de speltoestand geen
`0` waarde meer bevat.

## Scherm omschrijven

In de lijst van strings van het scherm zit het volgende formaat:

1. Conditie om iets te tonen/uitvoeren
2. Lijst met teksten en opmaak instructies
3. Afsluiting van conditie die begint met een `&`, en mutaties kan bevatten

Na het element dat start met een `&` volgt weer een conditie, tot het `"END"`
element tegen wordt gekomen.

Mutaties en condities zijn het zelfde voor scherm opmaak en speler acties.

## Acties omschrijven

In de lijst van strings van acties zit het volgende formaat:

1. Conditie om actie te tonen
2. Tekst om actie te omschrijven
3. Mutatie

Een actie bestaat dus ALTIJD uit 3 elementen. De lijst wordt verwerkt tot het
`"END"` element tegen wordt gekomen.

Mutaties en condities zijn het zelfde voor scherm opmaak en speler acties.

## Condities

Condities hebben het volgende formaat:

`index vergelijking waarde { ';', herhaling ... }`

Bijvoorbeeld:

`2=3;1=2`: Dit betekend, het getal op lokatie 2 in de speltoestand moet gelijk
zijn aan `3`, en getal op lokatie 1 in de speltoestand moet gelijk zijn aan `2`.
Waardes zijn altijd getallen.

Als aan deze conditie wordt voldaan, dan wordt de lijst van teksten getoond, en
de opmaak instructies uitgevoerd. En als er dan bij de afsluiter (element dat
begint met een `&`) mutaties gedefinieerd staan, dan worden deze uitgevoerd.

Soorten vergelijkingen:

- `=` waarde op speltoestand lokatie moet gelijk zijn aan gegeven getal waarde
- `!` waarde op speltoestand lokatie moet ongelijk zijn aan gegeven getal waarde
- `>` waarde op speltoestand lokatie moet groter zijn dan gegeven getal waarde
- `<` waarde op speltoestand lokatie moet kleiner zijn dan gegeven getal waarde

Je kan op deze manier zelf een representatie van iets maken op een getal lokatie
in de speltoestand. Bijvoorbeeld de lokatie waar je in het spel bevind, of
hoeveel geld je hebt, of de status van een voorwerp dat je bezit.

## Mutaties

Condities hebben het volgende formaat:

`index bewerking waarde { ';', herhaling ... }`

Bijvoorbeeld:

`2=3;1=2`: Dit betekend, het getal op lokatie 2 in de speltoestand wordt gelijk
gezet aan `3`, en getal op lokatie 1 in de speltoestand wordt gelijk gezet aan
`2`. Waardes zijn altijd getallen.

Soorten bewerkingen:

- `=` waarde op speltoestand lokatie wordt gelijk gezet aan gegeven getal waarde
- `+` waarde op speltoestand lokatie wordt verhoogd met gegeven getal waarde
- `-` waarde op speltoestand lokatie wordt verlaagd met gegeven getal waarde

## Opmaak

Een opmaak element kan worden gebruikt in de scherm opbouw, en begint altijd met
een `*`. Hiermee wordt het onderscheid gemaakt tussen een opmaak element en een
tekst element.

Opmaak heeft het volgende formaat:

`'*' instructie-type gegevens`

Een instructie type is altijd maar 1 karakter.

Ondersteunde instructie typen:

- `c` Wijzig de kleur. kleuren 1 tot en met 15 zijn te gebruiken. Zie
  https://en.wikibooks.org/wiki/QBasic/Text_Output#Color_by_Number
- `s` Wachten. gevolgd door een getal, wat staat voor de tijdsduur, in hele
  seconden.

## Tekst

Tekst elementen beginnen niet met een `*` of een `&`. Deze kunnen gewoon tekst
bevatten. 1 element tekst staat voor 1 regel op het scherm.

Bepaalde karakter reeksen worden in tekst automatisch vervangen:

- `$n` wordt vervangen door de naam van de speler
- `$H`/`$h` wordt vervangen door respectievelijk Hij/Zij of hij/zij
- `$Z`/`$z` wordt vervangen door respectievelijk Zijn/Haar of zijn/haar

Waardes uit de speltoestand in tekst gebruiken:

- `#12` Hiermee wordt `#12` vervangen door de getalwaarde op lokatie 12 in de
  speltoestand. De lokatie in tekst moet altijd worden aangegeven door 2
  cijfers. Om een waarde in het lokatie gebied 0-9 te tonen, moet je een
  voorloop nul gebruiken (`#04`).
