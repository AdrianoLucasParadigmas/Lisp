;Cria lista para teste:
(setq bd1 (cons(cons 'D1 (cons (cons '(A1 A2 A3)
(cons '(P1 P2 P3) 'nil))'Curso))'nil))

;Saida
;((D1 ((A1 A2 A3) (P1 P2 P3)) .CURSO)) 

Curso=      (cddar bd1)
Disciplina= (caar bd1)
Alunos=  	(caadar bd1)
Professor=  (car(cdadar bd1))


;INICIO---------------------------------------------------------------------
(setq bd1 'nil)

;MATRICULA COM BD1 VAZIA----------------------------------------------------
(defun LISTA_VAZIA(ALUNOS DISCIPLINAS BD)
	(if(null (cdr DISCIPLINAS))

		(cons(cons(car DISCIPLINAS) (cons(cons ALUNOS 
		(cons(cons 'nil 'nil)'nil)) 'nil))'nil)
		
		(cons(cons(car DISCIPLINAS)(cons(cons ALUNOS(cons(cons 'nil 'nil)
		'nil))'nil))(LISTA_VAZIA ALUNOS(cdr DISCIPLINAS)(cdr BD)))
		
	)
)

;BUSCA ALUNO----------------------------------------------------------------
(defun BUSCA_ALUNO(ALUNO BD)
	(if (null (car BD))
		nil
		(if(eql ALUNO (car BD))
			t
			(BUSCA_ALUNO ALUNO (cdr BD))
		)
	)
)

;INSERE ALUNO---------------------------------------------------------------
(defun INSERE_ALUNO (ALUNO DISCIPLINA BD)
	(if (eql (caar BD) DISCIPLINA)
		(if (null(caadar BD)); NAO ESXISTE ALUNO 
			
			(cons(cons DISCIPLINA (cons(cons(cons ALUNO 'nil)
			(cons(car(cdadar BD))'nil))(cddar BD)))'nil)			
			(if (BUSCA_ALUNO ALUNO (caadar BD))
				(cons (car BD) (cdr BD))
			
				(cons(cons DISCIPLINA(cons(cons(cons ALUNO(caadar BD))
				(cons(car(cdadar BD))'nil))(cddar BD)))(cdr BD));INSERE NO INICIO DA LISTA
			)
		)
		(if(null(car BD)); SEM DISPLINAS CADASTRADAS
			(cons(cons DISCIPLINA (cons(cons(cons ALUNO 'nil )'nil)'nil))'nil); DISCIPLINA NÃO EXISTE
			
			(cons (car BD) (INSERE_ALUNO ALUNO DISCIPLINA (cdr BD))) ; PROXIMA DISC
		)
	)
)

;PERCORRE LISTA DE ALUNOS---------------------------------------------------
(defun PERCORRE_ALUNO (ALUNOS DISCIPLINA BD) 
	(IF(NULL (car ALUNOS))
		(cons (car BD) (cdr BD)) 
		(PERCORRE_ALUNO (cdr ALUNOS) DISCIPLINA  
			(INSERE_ALUNO (car ALUNOS) DISCIPLINA  BD))
	)
)

;---------------------------------------------------------------------------


;FUNCAO PRINCIPAL MATRICULA-------------------------------------------------
(defun MATRICULAR (ALUNOS DISCIPLINAS BD)
	(if (null BD)
		(LISTA_VAZIA ALUNOS DISCIPLINAS BD)	
		(if(null (car DISCIPLINAS))
			(cons (car BD)(cdr BD))
			
			(MATRICULAR ALUNOS (cdr DISCIPLINAS) 
			(PERCORRE_ALUNO ALUNOS (car DISCIPLINAS)  BD)) 		
		)
	)
)

;TESTES---------------------------------------------------------------------
(setq bd1 (Matricular '(A1 A2 A3) '(D1 D2 D3) BD1))

(Matricular '(A1 A2 A3) '(D1 D2 D3) BD1)

;REMOVE DUPLICADOS----------------------------------------------------------





____________________________________________________________________________





