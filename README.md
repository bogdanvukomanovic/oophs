# Specifikacija ispitnog projekta: Implementacija objektno-orijentisanog programiranja u Haskell-u

Potrebno je napisati biblioteku u Haskell-u koja simulira jedan deo objektno-orijentisanog programiranja. Potrebno je implementirati učitavanje OO modela iz tekstualnog fajla koji je napisan u posebnom specifikacionom jeziku.

## OOP metamodel

Klasa ima naziv i pripada nekom paketu. Klasa ima konstruktore, polja i veze asocijacije.

Polja imaju naziv, tip, mogu imati fleg *notNull* koji znači da je polje obavezno (mora imati vrednost) i informaciju da li je polje kolekcija. Polja takođe imaju i listu dozvoljenih metoda, to može biti *Get*, *Set* i *Add* za kolekcije. Polja kolekcije ne mogu imati oznaku *notNull*.

Asocijacije su veze sa drugim klasama i mogu biti kardinaliteta *OneToMany*, *ManyToOne*, *OneToOne* i *ManyToMany*. Asocijacije imaju naziv kao i obična polja, oznaku kardinaliteta, mogu imati oznaku *owner* ako je taj kraj asocijacije vlasnik veze, što znači da se objektu posmatrane klase mogu postavljati povezani objekti, samo jedna strana asocijacija može biti vlasnik veze.

Kao vrednost konstruktora klasa može imati sledeće elemente:
- ***NoArgsConstructor*** - konstruktor bez argumenata
- ***RequiredArgsConstructor*** - konstruktor koji kao argumente dobija samo obavezna polja
- ***AllArgsConstructor*** - uzima kao argumente sva polja koji nisu kolekcije i asocijacije

Projekat treba da obezbedi kreiranje objekata klasa. Objekat ima podatak o klasi čija je instanca i vrednosti polja i asocijacija.
Za svako polje i asocijaciju se čuva naziv polja ili asocijacije iz klase čiji je objekat instanca i vrednost koja može biti jedan podatak ili kolekcija u zavisnosti od toga da li je polje kolekcija, odnosno u zavisnosti od kardinaliteta asocijacije. Vrednosti bilo kog prostog tipa int, float, boolean treba upisivati kao stringove u Haskell-u, a poseban zahtev će biti provera ispravnosti tipa. Objekat može biti i *Null*.

## Jezik za specifikaciju modela

Model klasa se učitava iz tekstualnog fajla sa ekstenzijom *.hclass* u kome je zapisana specifikacija više klasa iz modela. Na sledećem listingu prikazan je primer zapisa jedne klase.

	class Student p:studenti
	ime String @get @set @notNull
	prezime String @get @set @notNull
	ocene int * @get @set @add
	indeksi @OneToMany StudIndeks @owner @get @add
	constructor @NoArgsConstructor @RequiredArgsConstructor

Prvo se navodi ključna reč *class* zatim naziv klase i oznaka *p:* iza koje ide naziv paketa. Slede specifikacije polja, a zatim i specifikacije asocijacija, svaka u posebnom redu.

Za polje sa navodi naziv, tip, ako je polje kolekcija piše se zvezdica (\*) zatim idu dozvoljene metode sa oznakom @. Ako polje nije kolekcija može imati oznaku *@NotNull* koja znači da je polje obavezno.

Za asocijacije se piše naziv, oznaka kardinaliteta, podatak da li je vlasnik (*@owner*) i spisak dozvoljenih metoda.

Podaci o konstruktorima klase navode se u novom redu koji započinje ključnom rečju *constructor* (ne može postojati polje sa imenom constructor).

## Operacije sa objektima i promena stanja

Projekat treba da obezbedi operacije za kreiranje objekata i poziv dozvoljenih metoda za polje (*set*, *get* ili *add*). Neke od ovih metoda menjaju stanje objekata što je potrebno realizovati na odgovarajući način u Haskell-u, korišćenjem ideje State monade. Može se koristiti ugrađena monada ili napraviti posebnu monadu čije će stanje biti objekat.

Jedan način definisanja tipa koji se može implementirati kao monada:

	newtype ObjectState a = ObjectState { runObjectState :: Objekat -> (a,Objekat) }

Funkcija za kreiranje objekta uzima klasu koja se instancira i vrednosti argumenata konstrukora (lista stringova), potrebno je proveriti da li broj argumenata odgovara definisanom konstruktoru za klasu (na primer ako klasa ima samo *RequiredArgsConstructor* broj argumenata treba da odgovara broju obaveznih polja, ako klasa ima *NoArgsConstructor* može se proslediti prazna lista argumenata). Ako broj argumenata ne odgovara definisanom konstrukotru funkcija vraća *Null* objekat. Prilikom kreiranja objekta ne dodeljuju se vrednost kolekcijama i asocijacijama.

Funkcija *set* treba da postavi vrednost polju i potrebno je implementirati za obična polja, polja kolekcije i asocijacije. Funkcija *get* treba da vrati vrednost polja kao prvi element tupla u State monadi i takođe treba da bude implementirana za sve vrste polja. Funkcija *add* dodaje novi element u polje koje je kolekcija i asocijacije sa kardinalitetom *..toMany*. U implementaciji ovih funkcija proveravati da li su operacije dozvoljene po specifikaciji polja klase, a kod asocijacije proveriti *owner* fleg.

Kada se operacije pravilno definišu kao State monade, moguće je napisati ovakav primer jedne složenije manipulacije sa objektima:

	oopoperacija :: ObjectState String
	oopoperacija = do
				kreirajObjekat klasaStudent ["Milica", "Marić"]
				ime <- getPolje "ime"
				addPoljeLista "ocene" "10"
				addPoljeLista "ocene" "9"
				if ime == "Marija" then
					setPolje "prezime" "Novo Marija prezime"
				else
					setPolje "prezime" "Novo prezime"
				prezime <- getPolje "prezime"
				return prezime

  
Rezultat ove operacije kada se pozove sa *runObjectState* treba da vrati tuple:

	("Novo prezime", Objekat ….)

,  gde je prvi element konačan rezultat operacije (promenjeno prezime), a drugi je trenutno stanje objekta u koji su upisani ime, prezime i ocene.
