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

(defn ++ [atm]
  (swap! atm inc))

(defn add! [atm v]
  (swap! atm + v))

(defn statement [customer]
  (let [total-amount (atom 0)
        frequent-renter-points (atom 0)
        result (atom (str "Rental record for " (:name customer) "\n"))]
    (doseq [rental (:rentals customer)]
      (let [amount (atom 0.0)]
        (condp = (-> rental :movie :price-code)
          REGULAR (do
                    (add! amount 2)
                    (when (> (:days-rented rental) 2)
                      (add! amount (* (- (:days-rented rental) 2) 1.5))))
          NEW_RELEASE (do
                        (add! amount (* (:days-rented rental) 3)))
          CHILDREN (do
                     (add! amount 1.5)
                     (when (> (:days-rented rental) 3)
                       (add! amount (* (- (:days-rented rental) 3) 1.5)))))
        (++ frequent-renter-points)

        (when (and (= (-> rental :movie :price-code) NEW_RELEASE)
                   (> (-> rental :days-rented) 1))
          (++ frequent-renter-points))

        (swap! result str
               "\t" (-> rental :movie :title) "\t" @amount "\n")
        (add! total-amount @amount)))

    (swap! result str
           "Amount owed is " @total-amount "\n"
           "You earned " @frequent-renter-points " frequent renter points")
    @result))

(def sample (Customer. "First Customer"
                       [(Rental. (Movie. "new1" NEW_RELEASE) 1)
                        (Rental. (Movie. "new2" NEW_RELEASE) 2)
                        (Rental. (Movie. "reg1" REGULAR) 1)
                        (Rental. (Movie. "reg2" REGULAR) 3)
                        (Rental. (Movie. "kid1" CHILDREN) 1)
                        (Rental. (Movie. "kid2" CHILDREN) 4)]))