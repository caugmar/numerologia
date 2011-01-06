(ns numerologia.core)

(defn -main [] (println "Olá, mundo!"))

(defn separar-digitos [numero] (map (fn [c] (Integer. (str c))) (str numero)))

(defn criar-tabela [origem destino] 
  (let [letras (map (fn [c] c) origem)
        valores (separar-digitos destino)]
    (apply array-map (interleave letras valores))))

(def alfabeto-completo (criar-tabela "abcdefghijklmnopqrstuvwxyz"
                                     "12345678912345678912345678"))

(def consoantes (criar-tabela "abcdefghijklmnopqrstuvwxyz"
                              "02340678012345078912045678"))

(def vogais (criar-tabela "abcdefghijklmnopqrstuvwxyz"
                          "10005000900000600000300000"))

(def sem-acento (apply array-map (map (fn [c] c) "áaéeíióoúuàaèeìiòoùuâaêeîiôoûuãaẽeĩiõoũuäaëeïiöoüuçcñn")))

(def pontuação #{\space \' \! \@ \# \$ \% \¨ \& \* \( \) \_ \+ \- \= \` \´ \{ \[ \^ \~ \} \] \ª \º \< \> \, \. \: \; \? \/ \| \" \\ })

(defn converter-nome [nome] (remove pontuação (map (fn [c] (get sem-acento c c)) (.toLowerCase nome))))

(defn reduzir [numeros]
  (let [resultado (apply + numeros)]
    (if (or
          (and (>= resultado 1) (<= resultado 9))
          (= resultado 11)
          (= resultado 22))
      resultado
      (reduzir (separar-digitos resultado)))))

(println (reduzir (map alfabeto-completo (converter-nome "Carlos Augusto Marcicano"))))

