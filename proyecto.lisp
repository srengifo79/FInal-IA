;Proyecto
;Algoritmo Genetico
;Autor: Santiago Rengifo

;Algoritmo genetico para la busqueda de reglas de clasificacion para el problema de Haberman Survival.
;Se recurre al metodo planteado en: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.259.1812&rep=rep1&type=pdf

;El algoritmo se basara en las siguientes operaciones logicas y el algoritmo planteado en el taller 2:
;0: =
;1: !=
;2: <  
;3: <=
;4: >
;5: <=

(load "split-sequence.lisp")

;1. Lectura y entendimiento de datos:

;Lee el archivo y retorna una lista de listas con los casos.
;dataSet = refArchivo, line = linea leida
(defun readFile(dataSet line)
	(cond
		((null (car line)) (list))
		(t 
			(append 
				(list (mapcar (lambda (a) (parse-integer a)) line))
				(readFile dataSet (split-sequence:SPLIT-SEQUENCE #\, (read-line dataSet nil nil)))
			)
		)
	)
)

;Retorna los valores minimos entre dos casos.
;dataSet = individuo del dataSet, actual = minimos actuales
(defun compareMin (dataSet actual)
	(cond 
		((null dataSet) (list))
		(t 	(cons 
				(min (car dataSet) (car actual))
				(compareMin (cdr dataSet) (cdr actual))
			)
		)
	)
)

;Retorna los valores maximos entre dos casos.
;dataSet = individuo del dataSet, actual = maximos actuales
(defun compareMax (dataSet actual)
	(cond 
		((null dataSet) (list))
		(t 	(cons 
				(max (car dataSet) (car actual))
				(compareMax (cdr dataSet) (cdr actual))
			)
		)
	)
)

;Obtiene los valores maximos y minimos del conjunto de datos para cada variable.
;dataSet = datos, actualMin = minimos actuales, actualMax = maximos actuales
(defun minMaxVariables(dataSet actualMin actualMax) 
	(cond 
		((null dataSet) (list actualMin actualMax))
		(t (minMaxVariables (cdr dataSet) (compareMin (car dataSet) actualMin) (compareMax (car dataSet) actualMax)))
	)
)

;2. Create a random population of n individuals:

;Random number between range
(defun randomFromRange (start end)
  	(+ start (random (+ 1 (- end start))))
)

;Genetic Material:
;genera una regla 
;n = numero de variables de entrada, minValues = Valores minimos variables, maxValues = valores maximos variables 
(defun generateRule(n minValues maxValues)        
    (cond 
        ((= n 0) (list)) 
        (t 	(cons 
        		(list (random 6) (randomFromRange (car minValues) (car maxValues))) 
        		(generateRule (- n 1) (cdr minValues) (cdr maxValues))
        	)
        )
    )
)

;
;n = numero de variables de entrada, tGen = size of generation, minMaxValues = Valores minimos y maximos de las variables
(defun generateIndividuals(n tGen minMaxValues)
    (cond
        ((= tGen 1) 
        	(list (generateRule n (car minMaxValues) (car (cdr minMaxValues))))
        )
        (t (append (generateIndividuals n (- tGen 1) minMaxValues) (list (generateRule n (car minMaxValues) (car (cdr minMaxValues))))))
    )
)

;2. Draw: Calculate fitness

;Entrada: dataSetInstance, ind = individuo 
;Salida: Instacia del dataSet despues de aplicadas operaciones.
(defun applyRule(dataSetInstance ind)
	(cond
		((null ind) (list))
		((= (car (car ind)) 0)
			(if (= (car dataSetInstance) (car (cdr (car ind)))) 
				(cons 1 (applyRule (cdr dataSetInstance) (cdr ind)))
				(cons 0 (applyRule (cdr dataSetInstance) (cdr ind)))
			)
		)
		((= (car (car ind)) 1)
			(if (/= (car dataSetInstance) (car (cdr (car ind)))) 
				(cons 1 (applyRule (cdr dataSetInstance) (cdr ind)))
				(cons 0 (applyRule (cdr dataSetInstance) (cdr ind)))
			)
		)
		((= (car (car ind)) 2)
			(if (< (car dataSetInstance) (car (cdr (car ind)))) 
				(cons 1 (applyRule (cdr dataSetInstance) (cdr ind)))
				(cons 0 (applyRule (cdr dataSetInstance) (cdr ind)))
			)
		)
		((= (car (car ind)) 3)
			(if (<= (car dataSetInstance) (car (cdr (car ind)))) 
				(cons 1 (applyRule (cdr dataSetInstance) (cdr ind)))
				(cons 0 (applyRule (cdr dataSetInstance) (cdr ind)))
			)
		)
		((= (car (car ind)) 4)
			(if (> (car dataSetInstance) (car (cdr (car ind)))) 
				(cons 1 (applyRule (cdr dataSetInstance) (cdr ind)))
				(cons 0 (applyRule (cdr dataSetInstance) (cdr ind)))
			)
		)
		((= (car (car ind)) 5)
			(if (>= (car dataSetInstance) (car (cdr (car ind)))) 
				(cons 1 (applyRule (cdr dataSetInstance) (cdr ind)))
				(cons 0 (applyRule (cdr dataSetInstance) (cdr ind)))
			)
		)
		;(t (if (null (car word1)) (applyRule (cdr word1) (cdr word2) (cdr ind)) (cons (car word1) (applyRule (cdr word1) (cdr word2) (cdr ind)))))
	)
)

;clasifica dado un individuo
;instance = instancia del dataSet despues de aplicado individuo
(defun classify(instance)
	(if (>= (apply '+ instance) 2) (list 1 ) (list 2))  
)

;ind = individual
;clasificar el dataset dado un individuo
(defun classifyDataSet(dataSet individual)
	(mapcar (lambda (a) (classify (applyRule a individual))) dataSet)
)

;para mostrar clasificaciones
(defun classifyDataSetPrint (dataSet individual)
	(print (mapcar (lambda (a) (classify (applyRule a individual))) dataSet))
)

;clasificacion real
(defun getRealClasification(dataSet)
	(car (cdr (cdr (cdr (car dataSet)))))
)

;Comparar respuestas 
(defun compareAnswers(dataSet individualClasification) 
	(cond 
		((null individualClasification) 0)
		((= (getRealClasification dataSet) (car (car individualClasification)))
			(+ 1 (compareAnswers (cdr dataSet) (cdr individualClasification)))
		)
		(t (compareAnswers (cdr dataSet) (cdr individualClasification)))
	)
)

;funcion de fitness, se comparan las clasificaciones y se toma el numero de aciertos
(defun fitnessFunction(dataSet individual)
	(compareAnswers dataSet (classifyDataSet dataSet individual))
)

;calcula la tasa de acierto y lo propone como fitness
(defun calculateFitness(dataSet individuals) 
	(mapcar (lambda (a) (/ (fitnessFunction dataSet a) (coerce (list-length dataSet) 'float))) individuals)
	;shows all fitness of the generation
	;(mapcar (lambda (a) (print (fitnessFunction word1 word2 a))) individuals)
)

;Joins fitness and individuals so they can be sorted
(defun joinInfFit(individuals fitness)
	(cond 
		((null individuals) individuals)
		(t 
			(cons (list (car fitness) (car individuals)) (joinInfFit (cdr individuals) (cdr fitness)))	
		)
	)
)


;Split fitness and individuals to calcluate new fitness 
(defun splitInfFit(individuals) 
	(cond 
		((null individuals) individuals)
		(t (cons (car (cdr (car individuals))) (splitInfFit (cdr individuals))))
	)
)

;Ordena los individuos de mayor a menor fitness
(defun sortGeneration(dataSet individuals)
	(splitInfFit (sort (joinInfFit individuals (calculateFitness dataSet individuals)) #'> :key #'car))
)

;3. Reproduction

;reproduces 2 individuals resulting in 1 new individual
;se reproduce de manera aleatoria, con mayor probabilidad de obtener genes del de mayor fitness
(defun reproduce(ind1 ind2 pMut)
	(setq num (random 1.0))
	(cond
		((null ind1) ind1)
		((<= num pMut)												;mutation
			(cons (cons (random 6) (cdr (car ind1))) (reproduce (cdr ind1) (cdr ind2) (- pMut pMut)))
			;print if mutation 
			;(print "hi")
		)
		((<= num 0.7)													;70% posibilidad al ind con mayor fitness 
			(cons (car ind1) (reproduce (cdr ind1) (cdr ind2) pMut))
		)
		(t (cons (car ind2) (reproduce (cdr ind1) (cdr ind2) pMut)))
	)
)

;Intento dejar el 50% de los individuos sobrevivan para generar nuevos individuos
(defun numSurvivalInd(numIndividuals) 
	(cond
		((= (rem numIndividuals 2) 0)
			(/ numIndividuals 2)
		)
		(t (/ (- numIndividuals 1) 2))
	)
)

;get new individuals to be added to population, numInd = number of new individuals to be reproduced
(defun reproduceNewIndividuals(individuals numInd pMut) 
	(cond 
		((= 0 numInd) nil)
		(t (cons (reproduce (car individuals) (car (cdr individuals)) pMut) (reproduceNewIndividuals (cdr (cdr individuals)) (- numInd 1) pMut)))
	)
)

;Removes lowest fitness individuals, survial: number of individuals thar survives
(defun removeLowIndividuals(individuals survival)
	(cond
		((= survival 0) nil)
		(t (cons (car individuals) (removeLowIndividuals (cdr individuals) (- survival 1))))
	)
)

;Creates a new generation from parents
;survial: number of individuals thar survives
(defun newGeneration(individuals pMut)
	(setq tGen (list-length individuals))
	(setq survival (- tGen (numSurvivalInd (list-length individuals))))
	(setq trimedPopulation (removeLowIndividuals individuals survival))
	(append trimedPopulation (reproduceNewIndividuals individuals (- tGen survival) pMut))
)

;tGen = size of generation, pMut = probability of mutation, numGen = number of generations, k = min number of operations
(defun populate(dataSet individuals pMut numGen)
	(cond
		((= numGen 0) (car (sortGeneration dataSet individuals)))
		(t (populate dataSet (newGeneration (sortGeneration dataSet individuals) pMut) pMut (- numGen 1)))
	)
)

;"haberman.data"
;tGen = Tamaño Generacion, fileRoute = ruta archivo de datos
(defun start(tGen numGen pMut fileRoute)
	(setq pFile (open fileRoute))     ;Referencia Archivo
	(setq dataSet (readFile pFile (split-sequence:SPLIT-SEQUENCE #\, (read-line pFile nil nil))))	;Lectura de archivo, split y conversion a entero
	(setq minMaxValues (MinMaxVariables dataSet '(100 100 100 100) '(0 0 0 0)))		;Valores Maximos y Minimos
	(setq individuals (generateIndividuals 3 tGen minMaxValues))
	(setq answer (print (populate dataSet individuals pMut numGen)))
	(/ (fitnessFunction dataSet answer) (coerce (list-length dataSet) 'float))
)