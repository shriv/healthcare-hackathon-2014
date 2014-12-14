;; Scope of this script ;;
;; This script does the data intensive joining of the NHS prescriptions datasets.

;; More notes on the data taps ;;
;; The xls files from the folder "nhs-prescriptions-costs-aug13-aug14" are separately aggregated ans uploaded as a big csv on S3. This big csv is the drug-info file.
;; Files for the presription and presciber taps are labelled as TYYYYMMPDPI BNFT.CSV ;; and TYYYYMMADDR BNFT.CSV where YYYY is year and MM is month

(ns cascalog-demo.core
  (:use [cascalog.api]
        [cascalog.more-taps :only (hfs-delimited)])
  (:require [clojure.string :as s]
            [cascalog.logic.ops :as c])
  (:gen-class))

;; Data transformation functions
(defn get-month
  [time-string]
  (->> time-string
      (s/reverse)
      (take 2)
      (s/join)
      (s/reverse)))

(defn get-year
  [time-string]
  (->> time-string
       (take 4)
       (s/join)))

(defn get-county-code
  [postcode]
  (->> postcode
       (re-find (re-pattern "[A-Z]+"))))

(defn get-district-code
  [postcode]
  (-> postcode
       (s/split #" ")
       (first)))

;; Three main data taps
;; prescription data: TYYYYMMPDPI BNFT.CSV 
;; prescriber address data: TYYYYMMADDR BNFT.CSV 
;; aggregated drug info data: aggregated from all xls files

(defn get-prescription-tap
  [src]
  (<- [?drug-code
       ?practice]
      ((hfs-delimited src
                      :delimiter ","
                      :skip-header? true) :> _ _ ?practice ?drug-code _ ?items _ _ _ _ _)))

(defn get-drug-info
  [src]
  (<- [?drug-code
       ?drug-type
       ?chemical]
      ((hfs-delimited src
                      :delimiter ","
                      :skip-header? true) :> ?drug-code ?drug-type ?chemical _ _ _ _ _ _ _ _ _)))

(defn get-prescriber-tap
  [src]
  (<- [?month
       ?year
       ?practice
       ?county-code
       ?district-code]
      ((hfs-delimited src
                      :delimiter ","
                      :skip-header? true) :> ?time ?practice ?practice-name ?name2 ?street-name ?town ?county ?postcode)
      (get-month ?time :> ?month)
      (get-year ?time :> ?year)
      (get-county-code ?postcode :> ?county-code)
      (get-district-code ?postcode :> ?district-code)))

(defn joining-taps
  [prescription-src drug-src prescriber-src]
  (<- [?county
       ;; ?district-code
       ?month
       ?year
       ?drug-type
       ?chemical
       ?item-count]
      ((get-prescription-tap prescription-src) :> ?drug-code ?practice)
      ((get-drug-info drug-src) :> ?drug-code ?drug-type ?chemical)
      ((get-prescriber-tap prescriber-src) :> ?month ?year ?practice ?county ?district-code)
      (c/count :> ?item-count)))

;; Run the following in repl on test data
;; (-main "path/to/prescription-data.csv" "path/to/drug-info.csv" "path/to/prescriber-dat.csv")
;; To run query on AWS, create uberjar. Send uberjar to S3 bucket. Use a tool like Lemur or Amazonica to launch EMR jobs. 
(defn -main
  [prescription-src drug-src prescriber-src]
  (?- (stdout)
      (joining-taps prescription-src drug-src prescriber-src)))
