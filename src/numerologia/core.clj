(ns numerologia.core)

(defn -main []
  (println "Olá, mundo!"))

(defn criar-tabela [origem destino] 
  (let [letras (map (fn [c] c) origem)
        valores (map (fn [c] (Integer. (str c))) destino)]
    (apply hash-map (interleave letras valores))))

(def alfabeto-completo (criar-tabela "abcdefghijklmnopqrstuvwxyz"
                                     "12345678912345678912345678"))

(def consoantes (criar-tabela "abcdefghijklmnopqrstuvwxyz"
                              "02340678012345078912045678"))

(def vogais (criar-tabela "abcdefghijklmnopqrstuvwxyz"
                          "10005000900000600000300000"))

(def sem-acento (apply hash-map (map (fn [c] c) "áaéeíióoúuàaèeìiòoùuâaêeîiôoûuãaẽeĩiõoũuäaëeïiöoüuçcñn")))

(def pontuação #{\space \' \! \@ \# \$ \% \¨ \& \* \( \) \_ \+ \- \= \` \´ \{ \[ \^ \~ \} \] \ª \º \< \> \, \. \: \; \? \/ \| \" \\ })

(defn converter-nome [nome]
  (map (fn [c] c) (.toLowerCase nome)))
