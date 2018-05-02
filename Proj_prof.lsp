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

;DICIPLINAS?----------------------------------------------------------------

(defun DICIPLINAS? (BD)
	
	
	(if (null (car bd))
		nil
		(DICIPLINAS? (cdr BD))
	)
)

;VINCULAR-------------------------------------------------------------------

;BUSCA PROF-----------------------------------------------------------------
(defun BUSCA_PROF(PROFESSOR BD_PROFS)
	(if (null (car BD_PROFS))
		nil
		(if(eql PROFESSOR (car BD_PROFS))
			t
			(BUSCA_PROF PROFESSOR (cdr BD_PROFS))
		)
	)
)
;INSERE PROF----------------------------------------------------------------
(defun INSERE_PROF (PROFESSOR DISCIPLINA BD)
	(if (eql (caar BD) DISCIPLINA)
		(if (null(car(cdadar bd))); NAO ESXISTE PROFESSOR 
			(cons(cons DISCIPLINA (cons(cons (caadar BD) 
			(cons(cons PROFESSOR 'nil)'nil))(cddar BD)))(cdr BD));********
			(if (BUSCA_PROF PROFESSOR (car(cdadar BD)))
				BD
			
				(cons(cons DISCIPLINA(cons(cons(caadar BD)
				(cons(cons PROFESSOR (car(cdadar BD)))'nil ))(cddar BD)))(cdr BD));INSERE NO INICIO DA LISTA
			)
		)
		(if(null(car BD)); SEM DISPLINAS CADASTRADAS
 (cons(cons DISCIPLINA (cons(cons (cons 'nil 'nil)
 (cons(cons PROFESSOR'nil)'nil))'nil))'nil) ; DISCIPLINA NÃO EXISTE
			
			(cons (car BD) (INSERE_PROF PROFESSOR DISCIPLINA (cdr BD))) ; PROXIMA DISC
		)
	)
)

;(vincular '(PF1 PF2 PF3) '(d4) BD1)

;PERCORRE LISTA DE PROFS----------------------------------------------------
(defun PERCORRE_PROF (PROFESSORES DISCIPLINA BD) 
	(IF(NULL (car PROFESSORES))
		BD
		(PERCORRE_PROF (cdr PROFESSORES) DISCIPLINA 
		(INSERE_PROF (car PROFESSORES) DISCIPLINA  BD))
	)
)

;FUNCAO VINCULAR------------------------------------------------------------
(defun VINCULAR (PROFESSORES DISCIPLINAS BD)
		(if(null (car DISCIPLINAS))
			BD
			(VINCULAR PROFESSORES (cdr DISCIPLINAS) 
			(PERCORRE_PROF PROFESSORES (car DISCIPLINAS)  BD)) 		
		)
)

;CANCELAR VINCULO-----------------------------------------------------------
(defun REMOVE_PROFESSOR_LISTA (PROFESSOR BD_PROFESSOR);BD_PROFESSOR RECEBE APENAS A LISTA DE PROFESSORES
	(if (null (car BD_PROFESSOR));A LISTA ESTA VAZIA?
		BD_PROFESSOR ;SIM! DEVOLVE A LISTA DE PROFESSORES SEM ALTERACAO
		(if(eql PROFESSOR (car BD_PROFESSOR));NAO! EH O PROFESSOR BUSCADO
			(cdr BD_PROFESSOR);SIM! DEVOLVE A LISTA AVANCANDO UMA POSICAO 
			(cons (car BD_PROFESSOR) (REMOVE_PROFESSOR_LISTA PROFESSOR 
			(cdr BD_PROFESSOR)));NAO! CHAMA A FUNCAO NOVAMENTE COM O PROXIMO PROFESSOR
		)
	)
)

;(REMOVE_PROFESSOR_LISTA 'P1 (car(cdadar bd1)))

;REMOVE PROFESSOR---------------------------------------------------------------
(defun REMOVE_PROFESSOR (PROFESSOR DISCIPLINA BD)
	(if (eql (caar BD) DISCIPLINA);EH A MATERIA BUSCADA?
		(if (null(car(cdadar BD)));A LISTA DE MATERIAS ESTA VAZIA?
			BD ;SIM! DEVOLVE O RESTANTE DA LISTA SEM ALTERACAO 	
		    (cons(cons DISCIPLINA(cons(cons (cons (caadar BD) 'nil)
			(REMOVE_PROFESSOR_LISTA PROFESSOR (car (cdadar BD))))
			(cddar BD)))(cdr BD));NAO! 			
		)		
		(if(null(car BD));CHEGOU NO FINAL DO BANCO DE DADOS?
			BD ;SIM! DEVOLVE O RESTANTE DA LISTA SEM ALTERACAO 			
			(cons (car BD) (REMOVE_PROFESSOR PROFESSOR DISCIPLINA (cdr BD)));NAO! CHAMA A FUNCAO NOVAMENTE COM A PROXIMA DISCIPLINA
		)		
	)
)

;PERCORRE LISTA DE PROFESSORES PARA DESMATRICULAR--------------------------------
(defun PERCORRE_PROF2 (PROFESSORES DISCIPLINA BD) 
	(IF(NULL (car PROFESSORES))
		BD 
		(PERCORRE_PROF2 (cdr PROFESSORES) DISCIPLINA  
			(REMOVE_PROFESSOR (car PROFESSORES) DISCIPLINA  BD))
	)
)


;REMOVE VINCULO----------------------------------------------------------
(defun REMOVER-VINCULO (PROFESSORES DISCIPLINAS BD) 
	(if(null (car DISCIPLINAS))
		BD		
		(REMOVER-VINCULO PROFESSORES (cdr DISCIPLINAS) 
		(PERCORRE_PROF2 PROFESSORES (car DISCIPLINAS)  BD)) 		
	)	
)

;MATRICULADOS?--------------------------------------------------------------

(defun MATRICULADOS? (DICIPLINA BD)

    (if (null (car BD))
        nil
        (if (eql DICIPLINA (caar BD))
            (caadar BD)
            (matriculados? DICIPLINA (cdr BD))
        )
    )
)

;VINCULADOS?----------------------------------------------------------------

(defun VINCULADOS? (DICIPLINA BD)

    (if (null (car BD))
        nil
        (if (eql DICIPLINA (caar BD))
            (car(cdadar BD))
            (vinculados? DICIPLINA (cdr BD))
        )
    )
)

;BUSCA ALUNO----------------------------------------------------------------
(defun BUSCA_ALUNO(ALUNO BD_ALUNOS)
	(if (null (car BD_ALUNOS))
		nil
		(if(eql ALUNO (car BD_ALUNOS))
			t
			(BUSCA_ALUNO ALUNO (cdr BD_ALUNOS))
		)
	)
)
;INSERE ALUNO---------------------------------------------------------------
(defun INSERE_ALUNO (ALUNO DISCIPLINA BD)
	(if (eql (caar BD) DISCIPLINA)
		(if (null(caadar BD)); NAO ESXISTE ALUNO 
			
			(cons(cons DISCIPLINA (cons(cons(cons ALUNO 'nil)
			(cons(car(cdadar BD))'nil))(cddar BD)))(cdr BD));********
			(if (BUSCA_ALUNO ALUNO (caadar BD))
				BD
			
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
		BD
		(PERCORRE_ALUNO (cdr ALUNOS) DISCIPLINA  
			(INSERE_ALUNO (car ALUNOS) DISCIPLINA  BD))
	)
)

;---------------------------------------------------------------------------



;FUNCAO PRINCIPAL MATRICULA-------------------------------------------------
(defun MATRICULAR (ALUNOS DISCIPLINAS BD)
		(if(null (car DISCIPLINAS))
			BD
			
			(MATRICULAR ALUNOS (cdr DISCIPLINAS) 
			(PERCORRE_ALUNO ALUNOS (car DISCIPLINAS)  BD)) 		
		)
)

(defun REMOVE_ALUNO_LISTA (ALUNO BD_ALUNOS);BD_ALUNOS RECEBE APENAS A LISTA DE ALUNOS
	(if (null (car BD_ALUNOS));A LISTA ESTA VAZIA?
		BD_ALUNOS ;SIM! DEVOLVE A LISTA DE ALUNOS SEM ALTERACAO
		(if(eql ALUNO (car BD_ALUNOS));NAO! EH O ALUNO BUSCADO
			(cdr BD_ALUNOS);SIM! DEVOLVE A LISTA AVANCANDO UMA POSICAO 
			(cons (car BD_ALUNOS) (REMOVE_ALUNO_LISTA ALUNO (cdr BD_ALUNOS)));NAO! CHAMA A FUNCAO NOVAMENTE COM O PROXIMO ALUNO
		)
	)
)

;(REMOVE_ALUNO_LISTA 'A1 (caadar bd1))

;REMOVE ALUNO---------------------------------------------------------------
(defun REMOVE_ALUNO (ALUNO DISCIPLINA BD)
	(if (eql (caar BD) DISCIPLINA);EH A MATERIA BUSCADA?
		(if (null(caadar BD));A LISTA DE MATERIAS ESTA VAZIA?
			BD ;SIM! DEVOLVE O RESTANTE DA LISTA SEM ALTERACAO 	
		    (cons(cons DISCIPLINA(cons(cons (REMOVE_ALUNO_LISTA ALUNO(caadar BD))
			(cons(car(cdadar BD))'nil))(cddar BD)))(cdr BD));NAO! 			
		)		
		(if(null(car BD));CHEGOU NO FINAL DO BANCO DE DADOS?
			BD ;SIM! DEVOLVE O RESTANTE DA LISTA SEM ALTERACAO 			
			(cons (car BD) (REMOVE_ALUNO ALUNO DISCIPLINA (cdr BD)));NAO! CHAMA A FUNCAO NOVAMENTE COM A PROXIMA DISCIPLINA
		)		
	)
)

;PERCORRE LISTA DE ALUNOS PARA DESMATRICULAR--------------------------------
(defun PERCORRE_ALUNO2 (ALUNOS DISCIPLINA BD) 
	(IF(NULL (car ALUNOS))
		BD 
		(PERCORRE_ALUNO2 (cdr ALUNOS) DISCIPLINA  
			(REMOVE_ALUNO (car ALUNOS) DISCIPLINA  BD))
	)
)


;CANCELA MATRICULA----------------------------------------------------------
(defun CANCELAR-MATRICULA (ALUNOS DISCIPLINAS BD) 
	(if(null (car DISCIPLINAS))
		BD		
		(CANCELAR-MATRICULA ALUNOS (cdr DISCIPLINAS) 
		(PERCORRE_ALUNO2 ALUNOS (car DISCIPLINAS)  BD)) 		
	)	
)

(CANCELAR-MATRICULA '(A1 A2 A3) '(D1 D2 D3) BD1)



;VINCULAR DISCIPLINA A CURSO------------------------------------------------
(defun VINCULAR-DISC-CURSO(DISCIPLINA CURSO BD)
	(if (eql (caar BD) DISCIPLINA);EH A MATERIA BUSCADA
		(cons(cons DISCIPLINA(cons(cons (caadar BD)	 ;|	
		(cons(car(cdadar BD))'nil))CURSO))(cdr BD));|VINCULA NOVA DISCIPLINA OU SOBRESCREVE ANTERIOR 
		(if(null(car BD));CHEGOU NO FINAL DO BANCO DE DADOS?
			BD ;SIM! DEVOLVE O RESTANTE DA LISTA SEM ALTERACAO 			
			(cons (car BD) (VINCULAR-DISC-CURSO DISCIPLINA CURSO (cdr BD)));NAO! CHAMA A FUNCAO NOVAMENTE COM A PROXIMA DISCIPLINA
		)		
	)	
)

(VINCULAR-DISC-CURSO 'D1 'CALCULO BD1)

;DISC-CURSO-----------------------------------------------------------------

(defun DISC-CURSO (DICIPLINA BD)

	(if (null (car bd))
		nil
		(if (eql DICIPLINA (caar BD))
			(cddar BD)
			(DISC-CURSO DICIPLINA (car BD))
		)
	)
)

;CURSA?---------------------------------------------------------------------

(defun CURSA? (ALUNO BD)

	(if (null (car BD))
		nil
		(if (BUSCA_ALUNO ALUNO (caadar BD)) ;Se houver (true) o aluno na mateira
			(cons (caar BD) (CURSA? ALUNO (cdr BD)))
			nil
		)
	)

)

;MINISTRA?------------------------------------------------------------------

(defun MINISTRA? (PROFESSOR BD)

	(if (null (car BD))
		nil
		(if (BUSCA_PROF PROFESSOR (car(cdadar BD))) ;Se houver (true) o PROFESSOR na mateira
			(cons (caar BD) (MINISTRA? PROFESSOR (cdr BD)))
			nil
		)
	)

)

;DICIPLINAS?----------------------------------------------------------------

(defun DICIPLINAS? (BD)

	(if (null (car BD))
		nil
		(cons (car(car bd)) (DICIPLINAS? (cdr BD)))
	)

)

;TESTES---------------------------------------------------------------------
(setq bd1 (Matricular '(A1 A2 A3) '(D1 D2 D3) BD1))
(setq bd1 (vincular '(P1 P2 P3) '(D1 D2 D3) BD1))

(Matricular '(A1 A2 A3) '(D1 D2 D3) BD1)





