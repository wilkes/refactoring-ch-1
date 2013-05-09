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

(defn extra-days [n days-rented]
  (if (> days-rented n)
    (* (- days-rented n) 1.5)
    0))

(defmethod rental-price REGULAR [rental]
  (+ 2 (extra-days 2 (:days-rented rental))))

(defmethod rental-price NEW_RELEASE [rental]
  (* (:days-rented rental) 3))

(defmethod rental-price CHILDREN [rental]
  (+ 1.5 (extra-days 3 (:days-rented rental))))

(defn bonus-point? [rental]
  (and (= (-> rental :movie :price-code) NEW_RELEASE)
       (> (-> rental :days-rented) 1)))

(defn rental-points [rental]
  (+ 1 (if (bonus-point? rental)
         1
         0)))

(defn sum-with [f coll]
  (reduce + 0 (map f coll)))

(defn statement-data [{:keys [name rentals]}]
  {:name name
   :rentals (for [rental rentals]
              {:title (-> rental :movie :title)
               :amount (rental-price rental)})
   :total-amount (sum-with rental-price rentals)
   :frequent-renter-points (sum-with rental-points rentals)})

(defn statement [customer]
  (let [data (statement-data customer)]
    (str "Rental record for " (:name data) "\n"
         (apply str
                (for [{:keys [title amount]} (:rentals data)]
                  (str "\t" title "\t" amount "\n")))
         "Amount owed is " (:total-amount data) "\n"
         "You earned " (:frequent-renter-points data) " frequent renter points")))

(def sample (Customer. "First Customer"
                       [(Rental. (Movie. "new1" NEW_RELEASE) 1)
                        (Rental. (Movie. "new2" NEW_RELEASE) 2)
                        (Rental. (Movie. "reg1" REGULAR) 1)
                        (Rental. (Movie. "reg2" REGULAR) 3)
                        (Rental. (Movie. "kid1" CHILDREN) 1)
                        (Rental. (Movie. "kid2" CHILDREN) 4)]))