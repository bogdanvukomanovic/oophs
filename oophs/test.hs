import Object
import Utility.State
import Grammar
import Validator


student :: Class
student = Class "Student" [] [] "studenti" ["@NoArgsConstructor", "@RequiredArgsConstructor"] [ime,prezime,ocene] [indeksiStudent]
ime :: Attribute
ime     = Attribute "ime"     "String"  True  False [Method "get" [], Method "set" []]
prezime :: Attribute
prezime = Attribute "prezime" "String"  True  False [Method "get" [], Method "set" []]
ocene :: Attribute
ocene   = Attribute "ocene"   "int"     False True  [Method "get" [], Method "set" [], Method "add" []]

indeksiStudent = Association "indeksi" "@OneToMany" "StudIndeks" True [Method "get" [], Method "add" []] ("Student","StudIndeks")

--

studIndeks :: Class
studIndeks = Class "StudIndeks" [] [] "studenti" ["@NoArgsConstructor", "@RequiredArgsConstructor"] [broj,godinaUpisa,aktivan] [studProgramA, studentA, polozeniPredmeti]
broj :: Attribute
broj        = Attribute "broj"        "int"     True  False [Method "get" [], Method "set" []]
godinaUpisa :: Attribute
godinaUpisa = Attribute "godinaUpisa" "int"     True  False [Method "get" [], Method "set" []]
aktivan :: Attribute
aktivan     = Attribute "aktivan"     "boolean" False False [Method "get" [], Method "set" []]

studProgramA :: Association
studProgramA      = Association "studProgram"      "@ManyToOne"    "StudProgram" True  [Method "get" [], Method "set" []] ("StudIndeks", "StudProgram")
studentA :: Association
studentA          = Association "student"          "@ManyToOne"    "Student"     False [Method "get" [], Method "set" []] ("StudIndeks","Student")
polozeniPredmeti :: Association
polozeniPredmeti  = Association "polozeniPredmeti" "@ManyToMany"  "Predmet"     True  [Method "get" [], Method "set" [], Method "add" []] ("StudIndeks","Predmet")

-- 

studProgram :: Class
studProgram = Class "StudProgram" [] [] "studenti" ["@NoArgsConstructor","@AllArgsConstructor"] [naziv, godinaAkreditacije] [predmeti, indeksi]
naziv :: Attribute
naziv               = Attribute "naziv"               "String"  True  False [Method "get" [], Method "set" []]
godinaAkreditacije :: Attribute
godinaAkreditacije  = Attribute "godinaAkreditacije"  "int"     False False [Method "get" [], Method "set" []]

predmeti :: Association
predmeti =  Association "predmeti"  "@OneToMany" "Predmet"    True  [Method "get" [], Method "add" []] ("StudProgram","Premet")
indeksi :: Association
indeksi  =  Association "indeksi"   "@OneToMany" "StudIndeks" False [Method "get" [], Method "add" []] ("StudProgram","StudIndeks")

--

predmet :: Class
predmet = Class "Predmet" [] [] "studenti" ["@NoArgsConstructor","@AllArgsConstructor"] [nazivPredmet, espb] [polozili, studProgramPredmet]
nazivPredmet :: Attribute
nazivPredmet = Attribute "naziv" "String" True False [Method "get" [], Method "set" []]
espb :: Attribute
espb        = Attribute "espb"  "int"    True False  [Method "get" [], Method "set" []]

polozili :: Association
polozili           = Association "polozili"     "@ManyToMany" "StudIndeks"  False [Method "get" [], Method "set" [], Method "add" []] ("Predmet", "StudIndeks")
studProgramPredmet :: Association
studProgramPredmet = Association "studProgram"  "@ManyToOne"  "StudProgram" False [Method "get" [], Method "set" []] ("Predmet", "StudProgram")

---

test_1 :: State Object (Maybe Value)
test_1 = do instantiate student ["Milica", "Maric"]
            ime <- get "ime"
            add "ocene" $ ATTRValue "10"
            add "ocene" $ ATTRValue "9"
            case ime of 
               Nothing -> error "Greska"
               (Just x) -> if x == ATTRValue "Marija" then
                              set "prezime" $ ATTRValue "Novo Marija prezime"
                           else
                              set "prezime" $ ATTRValue "Novo prezime"
            get "prezime"

---

test_2 :: State Object (Maybe Value)
test_2 = do instantiate student ["Bogdan", "Vukomanovic"]
            get "ime"
            add "ocene" $ ATTRValue "5"
            add "ocene" $ ATTRValue "6"
            add "ocene" $ ATTRValue "7"
            add "ocene" $ ATTRValue "1"
            get "ocene"

---

objPredmet :: Object
objPredmet = execState (instantiate predmet []) Null

objStudentskiIndeks :: Object
objStudentskiIndeks = execState (instantiate studIndeks [])  Null

test_3 :: State Object (Maybe Value)
test_3 = do instantiate student []
            add "indeksi" $ ASSOValue objStudentskiIndeks
            add "indeksi" $ ASSOValue objPredmet
            add "ocene" $ ATTRValue "5"
            get "indeksi"

---

test_4 :: State Object ()
test_4 = do instantiate student []
            set "ime"     $ ATTRValue "Bogdan"
            set "prezime" $ ATTRValue "Vukomanovic"
            set "ocene"   $ ATTRValues ["10", "7", "9"]
            -- set "ocene"   $ ATTRValues ["10", "7", "5"]

---

-- objStudent :: Object
-- objStudent = snd $ runState (instantiate student ["Bogdan", "Vukomanovic"]) Null

-- validate_1 :: Bool
-- validate_1 = validateObject objStudent 

-- objStudIndeks :: Object
-- objStudIndeks = snd $ runState (instantiate studIndeks ["125", "2018"]) Null


-- validate_2 :: Bool
-- validate_2 = validateObject objStudIndeks



test_5 :: State Object ()
test_5 = do instantiate student []
            set "ime"     $ ATTRValue "Bogdan"
            set "prezime" $ ATTRValue "Vukomanovic"
            set "ocene"   $ ATTRValues ["10", "7", "9"]
            add "ocene"   $ ATTRValue "8"
            add "ocene"   $ ATTRValue "1"

x :: Object
x = execState test_5 Null
y :: Bool
y = validateObject x


---

test_6 :: State Object (Maybe Value)
test_6 = do instantiate predmet []
            set "naziv" $ ATTRValue "ASP"
            imePredmet <- get "naziv"
            get "polozili"                -- Nothing
            return imePredmet 
            
---

test_7 :: State Object (Maybe Value)
test_7 = do instantiate studProgram ["Racunarske nauke", "1998"]
            naziv <- get "naziv"
            set "naziv" $ ATTRValue "Racunarske mreze"
            promenjenNaziv <- get "naziv"
            godinaAkreditacije <- get "godinaAkreditacije"
            return promenjenNaziv 


---

test_8 :: State Object ()
test_8 = do instantiate student []
            add "ocene" $ ATTRValue "5"
            add "ocene" $ ATTRValue "213"
            add "ocene" $ ATTRValue "29"

---

obj9' :: Object
obj9'  = execState (instantiate studIndeks []) Null
obj9'' :: Object
obj9'' = execState (instantiate studProgram []) Null

test_9 :: State Object ()
test_9 = do instantiate student []
            add "indeksi" $ ASSOValue obj9''        -- not ok
            add "indeksi" $ ASSOValue obj9'         -- ok

---

 
student_10     = execState (instantiate student ["Bogdan", "Vukomanovic"]) Null
studIndeks_10  = execState (instantiate studIndeks ["125", "2018"]) Null
studProgram_10 = execState (instantiate studProgram ["Racunarske nauke", "2000"]) Null
predmet_10a     = execState (instantiate predmet ["Funkcionalno programiranje", "6"]) Null
predmet_10b     = execState (instantiate predmet ["Uvod u bioinformatiku", "6"]) Null

test_10 = do put studIndeks_10
             set "aktivan" $ ATTRValue "True"
             set "student" $ ASSOValue student_10                 -- OK
             set "student" $ ASSOValue studIndeks_10              -- Forbidden
             set "studProgram" $ ASSOValue studProgram_10
             set "polozeniPredmeti" $ ASSOValue predmet_10a       -- Forbidden
             set "polozeniPredmeti" $ ASSOValues [predmet_10a] 
             add "polozeniPredmeti" $ ASSOValue predmet_10b       -- OK


result_10 = execState test_10 Null

             
