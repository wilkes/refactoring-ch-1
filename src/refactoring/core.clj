(ns refactoring.core)

(def ^:const CHILDREN 2)
(def ^:const REGULAR 0)
(def ^:const NEW_RELEASE 1)

(defrecord Movie [title price-code])
(defrecord Rental [movie days-rented])
(defrecord Customer [name rentals])

(defn add-rental [customer rental]
  (Customer. (:name customer)
             (conj (:rentals customer) rental)))

(defmulti rental-price (fn [rental] (-> rental :movie :price-code)))

(defmethod rental-price REGULAR [rental]
  (+ 2 (if (> (:days-rented rental) 2)
         (* (- (:days-rented rental) 2) 1.5)
         0)))

(defmethod rental-price NEW_RELEASE [rental]
  (* (:days-rented rental) 3))

(defmethod rental-price CHILDREN [rental]
  (+ 1.5 (if (> (:days-rented rental) 3)
           (* (- (:days-rented rental) 3) 1.5)
           0)))

(defn rental-points [rental]
  (+ 1
     (if (and (= (-> rental :movie :price-code) NEW_RELEASE)
              (> (-> rental :days-rented) 1))
       1
       0)))

(defn statement [customer]
  (let [total-amount (reduce + 0
                             (map rental-price (:rentals customer)))
        frequent-renter-points (reduce + 0
                                       (map rental-points (:rentals customer)))
        result (atom (str "Rental record for " (:name customer) "\n"))]
    (doseq [rental (:rentals customer)]
      (let [amount (rental-price rental)]
        (swap! result str
               "\t" (-> rental :movie :title) "\t" amount "\n")))

    (swap! result str
           "Amount owed is " total-amount "\n"
           "You earned " frequent-renter-points " frequent renter points")
    @result))

(def sample (Customer. "First Customer"
                       [(Rental. (Movie. "new1" NEW_RELEASE) 1)
                        (Rental. (Movie. "new2" NEW_RELEASE) 2)
                        (Rental. (Movie. "reg1" REGULAR) 1)
                        (Rental. (Movie. "reg2" REGULAR) 3)
                        (Rental. (Movie. "kid1" CHILDREN) 1)
                        (Rental. (Movie. "kid2" CHILDREN) 4)]))