;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure)

(import '(java.lang.reflect Modifier Constructor)
        '(clojure.asm ClassWriter ClassVisitor Opcodes Type)
        '(clojure.asm.commons Method GeneratorAdapter)
        '(clojure.lang IPersistentMap))

;(defn method-sig [#^java.lang.reflect.Method meth]
;  [(. meth (getName)) (seq (. meth (getParameterTypes)))])

(defn- non-private-methods [#^Class c]
  (loop [mm {}
         considered #{}
         c c]
    (if c
      (let [[mm considered]
            (loop [mm mm
                   considered considered
                   meths (concat
                          (seq (. c (getDeclaredMethods)))
                          (seq (. c (getMethods))))]
              (if meths
                (let [#^Method meth (first meths)
                      mods (. meth (getModifiers))
                      mk (method-sig meth)]
                  (if (or (considered mk)
                          (. Modifier (isPrivate mods))
                          (. Modifier (isStatic mods))
                          (. Modifier (isFinal mods)))
                    (recur mm (conj considered mk) (rest meths))
                    (recur (assoc mm mk meth) (conj considered mk) (rest meths))))
                [mm considered]))]
        (recur mm considered (. c (getSuperclass))))
      mm)))

(defn- ctor-sigs [super]
  (for [#^Constructor ctor (. super (getDeclaredConstructors))
        :when (not (. Modifier (isPrivate (. ctor (getModifiers)))))]
    (apply vector (. ctor (getParameterTypes)))))

(defn- escape-class-name [c]
  (.. (.getSimpleName c) 
      (replace "[]" "<>")))

(defn- overload-name [mname pclasses]
  (if (seq pclasses)
    (apply str mname (interleave (repeat \-) 
                                 (map escape-class-name pclasses)))
    (str mname "-void")))

;(distinct (map first(keys (mapcat non-private-methods [Object IPersistentMap]))))

(defn gen-class 
  "Generates compiled bytecode for a class with the given
  package-qualified cname (which, as all names in these parameters, can
  be a string or symbol). The gen-class construct contains no
  implementation, as the implementation will be dynamically sought by
  the generated class in functions in a corresponding Clojure
  namespace. Given a generated class org.mydomain.MyClass, methods
  will be implemented that look for same-named functions in a Clojure
  namespace called org.domain.MyClass. The init and main
  functions (see below) will be found similarly. The static
  initializer for the generated class will attempt to load the Clojure
  support code for the class as a resource from the claspath, e.g. in
  the example case, org/mydomain/MyClass.clj

  Returns a map containing :name and :bytecode. Most uses will be
  satisfied by the higher-level gen-and-load-class and
  gen-and-store-class functions, which generate and immediately load,
  or generate and store to disk, respectively.

  Options should be a set of key/value pairs, all of which are optional:

  :extends aclass

  Specifies the superclass, the non-private methods of which will be
  overridden by the class. If not provided, defaults to Object.

  :implements [interface ...]

  One or more interfaces, the methods of which will be implemented by the class.

  :init name

  If supplied, names a function that will be called with the arguments
  to the constructor. Must return [[superclass-constructor-args] state] 
  If not supplied, the constructor args are passed directly to
  the superclass constructor and the state will be nil

  :constructors {[param-types] [super-param-types], ...}

  By default, constructors are created for the generated class which
  match the signature(s) of the constructors for the superclass. This
  parameter may be used to explicitly specify constructors, each entry
  providing a mapping from a constructor signature to a superclass
  constructor signature. When you supply this, you must supply an :init
  specifier.

  :methods [[name [param-types] return-type], ...]

  The generated class automatically defines all of the non-private
  methods of its superclasses/interfaces. This parameter can be used
  to specify the signatures of additional methods of the generated
  class. Do not repeat superclass/interface signatures here.

  :main boolean

  If supplied and true, a static public main function will be
  generated. It will pass each string of the String[] argument as a
  separate argument to a function called 'main.

  :factory name

  If supplied, a (set of) public static factory function(s) will be
  created with the given name, and the same signature(s) as the
  constructor(s).
  
  :state name

  If supplied, a public final instance field with the given name will be
  created. You must supply an :init function in order to provide a
  value for the state. Note that, though final, the state can be a ref
  or agent, supporting the creation of Java objects with transactional
  or asynchronous mutation semantics.

  :exposes {protected-field-name {:get name :set name}, ...}

  Since the implementations of the methods of the generated class
  occur in Clojure functions, they have no access to the inherited
  protected fields of the superclass. This parameter can be used to
  generate public getter/setter methods exposing the protected field(s)
  for use in the implementation."

  [cname & options]
  (let [name (str cname)
        {:keys [extends implements constructors methods main factory state init exposes]} (apply hash-map options)
        super (or extends Object)
        interfaces implements
        supers (cons super (seq interfaces))
        ctor-sig-map (or constructors (zipmap (ctor-sigs super) (ctor-sigs super)))
        cv (new ClassWriter (. ClassWriter COMPUTE_MAXS))
        cname (. name (replace "." "/"))
        ctype (. Type (getObjectType cname))
        iname (fn [c] (.. Type (getType c) (getInternalName)))
        totype (fn [c] (. Type (getType c)))
        to-types (fn [cs] (if (pos? (count cs))
                            (into-array (map totype cs))
                            (make-array Type 0)))
        obj-type (totype Object)
        arg-types (fn [n] (if (pos? n)
                            (into-array (replicate n obj-type))
                            (make-array Type 0)))
        super-type (totype super)
        init-name (str init)
        factory-name (str factory)
        state-name (str state)
        main-name "main"
        var-name (fn [s] (str s "__var"))
        rt-type  (totype clojure.lang.RT)
        var-type  (totype clojure.lang.Var)
        ifn-type (totype clojure.lang.IFn)
        iseq-type (totype clojure.lang.ISeq)
        ex-type  (totype java.lang.UnsupportedOperationException)
        all-sigs (distinct (concat (map #(let[[m p] (key %)] {m [p]}) (mapcat non-private-methods supers))
                                   (map (fn [[m p]] {(str m) [p]}) methods)))
        sigs-by-name (apply merge-with concat {} all-sigs)
        overloads (into {} (filter (fn [[m s]] (rest s)) sigs-by-name))
        var-fields (concat (and init [init-name]) 
                           (and main [main-name])
                           (distinct (concat (keys sigs-by-name)
                                             (mapcat (fn [[m s]] (map #(overload-name m %) s)) overloads)
                                             (mapcat (comp (partial map str) vals val) exposes))))
        emit-get-var (fn [gen v]
                       (let [false-label (. gen newLabel)
                             end-label (. gen newLabel)]
                         (. gen getStatic ctype (var-name v) var-type)
                         (. gen dup)
                         (. gen invokeVirtual var-type (. Method (getMethod "boolean isBound()")))
                         (. gen ifZCmp (. GeneratorAdapter EQ) false-label)
                         (. gen invokeVirtual var-type (. Method (getMethod "Object get()")))
                         (. gen goTo end-label)
                         (. gen mark false-label)
                         (. gen pop)
                         (. gen visitInsn (. Opcodes ACONST_NULL))
                         (. gen mark end-label)))
        emit-forwarding-method
        (fn [mname pclasses rclass else-gen]
          (let [ptypes (to-types pclasses)
                rtype (totype rclass)
                m (new Method mname rtype ptypes)
                is-overload (overloads mname)
                gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)
                found-label (. gen (newLabel))
                else-label (. gen (newLabel))
                end-label (. gen (newLabel))]
            (. gen (visitCode))
            (when is-overload
              (emit-get-var gen (overload-name mname pclasses))
              (. gen (dup))
              (. gen (ifNonNull found-label))
              (. gen (pop)))
            (emit-get-var gen mname)
            (. gen (dup))
            (. gen (ifNull else-label))
            (when is-overload
              (. gen (mark found-label)))
                                        ;if found
            (. gen (loadThis))
                                        ;box args
            (dotimes i (count ptypes)
              (. gen (loadArg i))
              (. clojure.lang.Compiler$HostExpr (emitBoxReturn nil gen (nth pclasses i))))
                                        ;call fn
            (. gen (invokeInterface ifn-type (new Method "invoke" obj-type 
                                                  (into-array (cons obj-type 
                                                                    (replicate (count ptypes) obj-type))))))
                                        ;unbox return
            (. gen (unbox rtype))
            (when (= (. rtype (getSort)) (. Type VOID))
              (. gen (pop)))
            (. gen (goTo end-label))
            
                                        ;else call supplied alternative generator
            (. gen (mark else-label))
            (. gen (pop))
            
            (else-gen gen m)
            
            (. gen (mark end-label))
            (. gen (returnValue))
            (. gen (endMethod))))
        ]
                                        ;start class definition
    (. cv (visit (. Opcodes V1_5) (. Opcodes ACC_PUBLIC)
                 cname nil (iname super)
                 (when interfaces
                   (into-array (map iname interfaces)))))
    
                                        ;static fields for vars
    (doseq v var-fields
      (. cv (visitField (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_FINAL) (. Opcodes ACC_STATIC))
                        (var-name v) 
                        (. var-type getDescriptor)
                        nil nil)))
    
                                        ;instance field for state
    (when state
      (. cv (visitField (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_FINAL))
                        state-name 
                        (. obj-type getDescriptor)
                        nil nil)))
    
                                        ;static init to set up var fields and load clj
    (let [gen (new GeneratorAdapter (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_STATIC)) 
                   (. Method getMethod "void <clinit> ()")
                   nil nil cv)]
      (. gen (visitCode))
      (doseq v var-fields
        (. gen push name)
        (. gen push v)
        (. gen (invokeStatic rt-type (. Method (getMethod "clojure.lang.Var var(String,String)"))))
        (. gen putStatic ctype (var-name v) var-type))
      
      (. gen push ctype)
      (. gen push (str (. name replace \. (. java.io.File separatorChar)) ".clj"))
      (. gen (invokeStatic rt-type (. Method (getMethod "void loadResourceScript(Class,String)"))))
      
      (. gen (returnValue))
      (. gen (endMethod)))
    
                                        ;ctors
    (doseq [pclasses super-pclasses] ctor-sig-map
      (let [ptypes (to-types pclasses)
            super-ptypes (to-types super-pclasses)
            m (new Method "<init>" (. Type VOID_TYPE) ptypes)
            super-m (new Method "<init>" (. Type VOID_TYPE) super-ptypes)
            gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)
            no-init-label (. gen newLabel)
            end-label (. gen newLabel)
            nth-method (. Method (getMethod "Object nth(Object,int)"))
            local (. gen newLocal obj-type)]
        (. gen (visitCode))
        
        (if init
          (do
            (emit-get-var gen init-name)
            (. gen dup)
            (. gen ifNull no-init-label)
                                        ;box init args
            (dotimes i (count pclasses)
              (. gen (loadArg i))
              (. clojure.lang.Compiler$HostExpr (emitBoxReturn nil gen (nth pclasses i))))
                                        ;call init fn
            (. gen (invokeInterface ifn-type (new Method "invoke" obj-type 
                                                  (arg-types (count ptypes)))))
                                        ;expecting [[super-ctor-args] state] returned
            (. gen dup)
            (. gen push 0)
            (. gen (invokeStatic rt-type nth-method))
            (. gen storeLocal local)
            
            (. gen (loadThis))
            (. gen dupX1)
            (dotimes i (count super-pclasses)
              (. gen loadLocal local)
              (. gen push i)
              (. gen (invokeStatic rt-type nth-method))
              (. clojure.lang.Compiler$HostExpr (emitUnboxArg nil gen (nth super-pclasses i))))
            (. gen (invokeConstructor super-type super-m))
            
            (if state
              (do
                (. gen push 1)
                (. gen (invokeStatic rt-type nth-method))
                (. gen (putField ctype state-name obj-type)))
              (. gen pop))
            
            (. gen goTo end-label)
                                        ;no init found
            (. gen mark no-init-label)
            (. gen (throwException ex-type (str init-name " not defined")))
            (. gen mark end-label))
          (if (= pclasses super-pclasses)
            (do
              (. gen (loadThis))
              (. gen (loadArgs))
              (. gen (invokeConstructor super-type super-m)))
            (throw (new Exception ":init not specified, but ctor and super ctor args differ"))))

        (. gen (returnValue))
        (. gen (endMethod))
                                        ;factory
        (when factory
          (let [fm (new Method factory-name ctype ptypes)
                gen (new GeneratorAdapter (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_STATIC)) 
                         fm nil nil cv)]
            (. gen (visitCode))
            (. gen newInstance ctype)
            (. gen dup)
            (. gen (loadArgs))
            (. gen (invokeConstructor ctype m))            
            (. gen (returnValue))
            (. gen (endMethod))))))
    
                                        ;add methods matching supers', if no fn -> call super
    (let [mm (non-private-methods super)]
      (doseq #^java.lang.reflect.Method meth (vals mm)
             (emit-forwarding-method (.getName meth) (.getParameterTypes meth) (.getReturnType meth) 
                                     (fn [gen m]
                                       (. gen (loadThis))
                                        ;push args
                                       (. gen (loadArgs))
                                        ;call super
                                       (. gen (visitMethodInsn (. Opcodes INVOKESPECIAL) 
                                                               (. super-type (getInternalName))
                                                               (. m (getName))
                                                               (. m (getDescriptor)))))))
                                        ;add methods matching interfaces', if no fn -> throw
       (doseq #^Class iface interfaces
              (doseq #^java.lang.reflect.Method meth (. iface (getMethods))
                     (when-not (contains? mm (method-sig meth))
                       (emit-forwarding-method (.getName meth) (.getParameterTypes meth) (.getReturnType meth) 
                                               (fn [gen m]
                                                 (. gen (throwException ex-type (. m (getName)))))))))
                                        ;extra methods
       (doseq [mname pclasses rclass :as msig] methods
         (emit-forwarding-method (str mname) pclasses rclass 
                                 (fn [gen m]
                                     (. gen (throwException ex-type (. m (getName))))))))

                                        ;main
    (when main
      (let [m (. Method getMethod "void main (String[])")
            gen (new GeneratorAdapter (+ (. Opcodes ACC_PUBLIC) (. Opcodes ACC_STATIC)) 
                     m nil nil cv)
            no-main-label (. gen newLabel)
            end-label (. gen newLabel)]
        (. gen (visitCode))

        (emit-get-var gen main-name)
        (. gen dup)
        (. gen ifNull no-main-label)
        (. gen loadArgs)
        (. gen (invokeStatic rt-type (. Method (getMethod "clojure.lang.ISeq seq(Object)"))))
        (. gen (invokeInterface ifn-type (new Method "applyTo" obj-type 
                                              (into-array [iseq-type]))))
        (. gen pop)
        (. gen goTo end-label)
                                        ;no main found
        (. gen mark no-main-label)
        (. gen (throwException ex-type (str main-name " not defined")))
        (. gen mark end-label)
        (. gen (returnValue))
        (. gen (endMethod))))
                                        ;field exposers
    (doseq [f {getter :get setter :set}] exposes
      (let [fld (.getField super (str f))
            ftype (totype (.getType fld))]
        (when getter
          (let [m (new Method (str getter) ftype (to-types []))
                gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)]
            (. gen (visitCode))
            (. gen loadThis)
            (. gen getField ctype (str f) ftype)
            (. gen (returnValue))
            (. gen (endMethod))))
        (when setter
          (let [m (new Method (str setter) (. Type VOID_TYPE) (into-array [ftype]))
                gen (new GeneratorAdapter (. Opcodes ACC_PUBLIC) m nil nil cv)]
            (. gen (visitCode))
            (. gen loadThis)
            (. gen loadArgs)
            (. gen putField ctype (str f) ftype)
            (. gen (returnValue))
            (. gen (endMethod))))))
                                        ;finish class def
    (. cv (visitEnd))
    {:name name :bytecode (. cv (toByteArray))}))

(defn gen-and-load-class 
  "Generates and immediately loads the bytecode for the specified
  class. Note that a class generated this way can be loaded only once
  - the JVM supports only one class with a given name per
  classloader. Subsequent to generation you can import it into any
  desired namespaces just like any other class. See gen-class for a
  description of the options."

  [name & options]
  (let [{:keys [name bytecode]}
        (apply gen-class (str name) options)]
    (.. clojure.lang.RT ROOT_CLASSLOADER (defineClass (str name) bytecode))))

(defn gen-and-save-class 
  "Generates the bytecode for the named class and stores in a .class
  file in a subpath of the supplied path, the directories for which
  must already exist. See gen-class for a description of the options"

  [path name & options]
  (let [{:keys [name bytecode]} (apply gen-class (str name) options)
        file (java.io.File. path (str (. name replace \. (. java.io.File separatorChar)) ".class"))]
    (.createNewFile file)
    (with-open f (java.io.FileOutputStream. file)
      (.write f bytecode))))

(comment
;usage
(gen-class 
 package-qualified-name
  ;all below are optional
 :extends aclass
 :implements [interface ...]
 :constructors {[param-types] [super-param-types], }
 :methods [[name [param-types] return-type], ]
 :main boolean
 :factory name
 :state name
 :init name
 :exposes {protected-field {:get name :set name}, })
 
;(gen-and-load-class 
(clojure/gen-and-save-class 
 "/Users/rich/Downloads"
 'fred.lucy.Ethel 
 :extends clojure.lang.Box ;APersistentMap
 :implements [clojure.lang.IPersistentMap]
 :state 'state
                                        ;:constructors {[Object] [Object]}
                                        ;:init 'init
 :main true
 :factory 'create
 :methods [['foo [Object] Object]
           ['foo [] Object]]
 :exposes {'val {:get 'getVal :set 'setVal}})

(in-ns 'fred.lucy.Ethel__2276)
(clojure/refer 'clojure :exclude '(assoc seq count cons))
(defn init [n] [[] n])
(defn foo 
  ([this] :foo) 
  ([this x] x))
(defn main [x y] (println x y))
(in-ns 'user)
(def ethel (new fred.lucy.Ethel__2276 42))
(def ethel (fred.lucy.Ethel__2276.create 21))
(fred.lucy.Ethel__2276.main (into-array ["lucy" "ricky"]))
(.state ethel)
(.foo ethel 7)
(.foo ethel)
(.getVal ethel)
(.setVal ethel 12)

(gen-class org.clojure.MyComparator :implements [Comparator])
(in-ns 'org.clojure.MyComparator)
(defn compare [this x y] ...)

(load-file "/Users/rich/dev/clojure/src/genclass.clj")

(clojure/gen-and-save-class "/Users/rich/dev/clojure/gen/" 
 'org.clojure.ClojureServlet 
 :extends javax.servlet.http.HttpServlet)

)
