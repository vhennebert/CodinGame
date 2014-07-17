(ns codingame
 "Utilities to ease the development of CodinGame solutions.")

(defn- find-main [file]
  (load-file file)
  (let [possible-namespaces '(Solution Player)]
    (some #(try (ns-resolve % '-main) (catch Exception e nil)) possible-namespaces)))

(let [try-run #(try (%) (catch Exception e (.printStackTrace e)))]
  (defn run
    "Loads file and runs the main function defined in it.

    1 argument is the same as 2 arguments with the second being \"/tmp/in.txt\".

    With 2 arguments, the second argument is the name of a file to use as input.

    With 3 arguments, the second argument must be the :str keyword and the third argument must be a string to use as input."
    ([file]
     (run file "/tmp/in.txt"))
    ([file input]
     (let [main (find-main file)]
       (with-open [r (clojure.lang.LineNumberingPushbackReader.
                       (clojure.java.io/reader input))]
         (binding [*in* r]
           (try-run main)))))
    ([file string input]
     (assert (= :str string))
     (let [main (find-main file)]
       (with-in-str input (try-run main))))))

; Vim shortcut to quickly copy-paste solutions to the web console
; nmap _c :%y+<Enter>
