; Load an example or two using the new concrete representation.

; (use 'meta.kernel)
(use 'meta.core)

(def example1 
  (node :clojure/kernel/app 
    :expr 
    (node :clojure/kernel/lambda 
      :params [
      (node :clojure/kernel/bind
        :core/id :1),
      (node :clojure/kernel/bind
        :core/id :2)
      ]
          
      :body
      (node :clojure/kernel/app
        :expr
        (node :clojure/builtin/plus)
        
        :args [
        (node :clojure/kernel/var
          :ref 
          (node :core/ref 
            :id :1)),
        (node :clojure/kernel/var
          :ref 
          (node :core/ref 
            :id :2)),
        ]
        ))
        
      :args [
      (node :clojure/kernel/int 
        :value 3)
      (node :clojure/kernel/int
        :value 4)
      ]
      ))
