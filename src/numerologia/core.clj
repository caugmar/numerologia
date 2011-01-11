(ns numerologia.core
  (:use numerologia.textos))

(defn separar-digitos [numero] (map (fn [c] (Integer. (str c))) (str numero)))

(defn criar-tabela [origem destino] 
  (let [letras (seq origem)
        valores (separar-digitos destino)]
    (apply array-map (interleave letras valores))))

(def alfabeto-completo (criar-tabela "abcdefghijklmnopqrstuvwxyz"
                                     "12345678912345678912345678"))

(def consoantes (criar-tabela "abcdefghijklmnopqrstuvwxyz"
                              "02340678012345078912045678"))

(def vogais (criar-tabela "abcdefghijklmnopqrstuvwxyz"
                          "10005000900000600000300000"))

(def sem-acento (apply array-map (interleave (seq "áéíóúàèìòùâêîôûãẽĩõũäëïöüçñ")
                                             (seq "aeiouaeiouaeiouaeiouaeioucn"))))

(def pontuação #{\space \' \! \@ \# \$ \% \¨ \& \* \( \) \_ \+ \- \= \` \´
                   \{ \[ \^ \~ \} \] \ª \º \< \> \, \. \: \; \? \/ \| \" \\ })

(defn converter-nome [nome] (remove pontuação (map (fn [c] (get sem-acento c c)) (.toLowerCase nome))))

(defn reduzir [numeros]
  (let [resultado (apply + numeros)]
    (if (or
          (and (>= resultado 1) (<= resultado 9))
          (= resultado 11)
          (= resultado 22))
      resultado
      (reduzir (separar-digitos resultado)))))

(defn converter-data [data] (map #(Integer. (str %)) (remove (fn [c] (= c \/)) data)))

(defn obter-numero-do-caminho-da-vida [data] (reduzir (converter-data data)))

(defn obter-numero-do-destino [nome] (reduzir (map alfabeto-completo (converter-nome nome))))

(defn obter-numero-do-desejo-da-alma [nome] (reduzir (map vogais (converter-nome nome))))

(defn obter-numero-dos-sonhos-interiores [nome] (reduzir (map consoantes (converter-nome nome))))

(defn obter-numero-do-aniversario [data] (Integer. (apply str (take 2 data))))

