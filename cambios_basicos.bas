

' aqui NO AISLA grupos de comentarios con tipo REM1 o REM2, sino que CAMBIA el formato
' de los comentarios /**/ y // tipo C, en la posicion indicada por la rutina "aisla_grupos"
function comentarios(pp As Integer) As integer
	Dim ff As Integer
	Dim oo As Integer
	Dim As String sf
	
	
	' NOTA: encontre DOS fallos curiosos
	'       primero, si aparece un // dentro de un grupo /* */ , al actuar, desactiva el */ y el resto queda como visible
	'      segundo, es al reves, que si en un comentario // aparece un /* */, al cerrar el */, el resto queda visible
	Dim grupo As Integer=0 ' para evitar que un tipo cambie al otro (de // a /* o al reves)
	' para evitar el fallo, GRUPO=0 es FUERA de AMBOS tipos
	' GRUPO=1 es DENTRO del tipo /* */ (por lo tanto // no actua)
	' GRUPO=2 es DENTRO del tipo // (por lo tanto /* */ no actua)
	
	
	oo=ff ' para poner textos de error, si algo malo ocurre
	
	sf=leetexto(pp,2,0)
	
	' nota: en el idioma ingles, exiten los "apostrofes" tipo "'" (ejemplo Don't) que confunden al FB. DEBO cambiarlos por "´"
	
	If sf="/*" And grupo<>2 Then ' caso /* */
		grupo=1 'evitar que la parte que busca "//" altere esta, en casos tipo --> " /* pepe; //pipo; */ ", con comentario // dentro de /**/
		cambiatexto(pp+1,"'") ' cambio * por ' para construir un /' '/ en lugar de /* */
		For ff=pp+2 To tcar
			sf=leetexto(ff,2,0)
			If Left(sf,1)="'" Then 
				cambiatexto(ff,"´") ' para no confundir al FB con comentarios falsos dentro de comentarios
			EndIf
			If sf="*/" Then
				grupo=0 ' acabamos, y la parte "//" puede actuar si quiere
				cambiatexto(ff,"'")
				Return ff+2 ' hemos encontrado el final */
			EndIf
		Next
		' si se llega al final y no aparece el */ hay un problema
		pon_error("Error buscando comentarios /* */ en la linea:",oo)
	EndIf
	
	If sf="//" And grupo<>1 Then ' caso //, solo actua si NO estamos dentro de un grupo /* */
		grupo=2
		cambiatexto(pp,"''") ' añado un doble "''" para no descolocar el comentario (vamos, cambio // por '' )
		For ff=pp+2 To tcar
			sf=leetexto(ff,1,0)
			If sf="'" Then 
				cambiatexto(ff,"´") ' para no confundir al FB con comentarios falsos dentro de comentarios
			EndIf
			If sf=Chr(CRLF) Then 
				grupo=0 ' acabamos, y la parte "/* */" puede actuar si quiere
				Return ff ' hemos encontrado el final
			EndIf
		Next
		' en el caso de comentario // se permite llegar hasta el final, sin que de error
		' o sea, que puede ser justo la ultima linea del codigo, que sea un comentario
	EndIf	

	'If sf="''" Then ' si son comentarios ya cambiados antes, o sea, del tipo FreeBasic
	'	For ff=pp+2 To tcar
	'		sf=leetexto(ff,1,0)
	'		If sf="'" Then cambiatexto(ff,"´") ' para no confundir al FB con comentarios falsos dentro de comentarios
	'		If sf=Chr(CRLF) Then 
	'			Return ff ' hemos encontrado el final
	'		EndIf
	'	Next
	'EndIf	
	
	Return pp+1 ' si no hay comentarios, avanza posicion y vuelve sin mas
End Function


' marcamos comentarios // y /* */, los grupos de texto entre "" o '' y las directivas #
Sub aisla_grupos()
	
	Dim aa As Integer
	Dim ff As Integer
	Dim hh As Integer
	Dim ss As String
	Dim sa As String
	Dim sr As String
	Dim tipo_grupo As Integer=0
	Dim escrlf As Integer=0
	
	Dim APOSTROFES As Integer=0 ' caso especial de COMILLAS, pero entre APOSTROFES (tipo if(*p=='C') )
	
	''''''''''''''''''''''''''''''''''''''''
	' lo estudiamos, separando por caracteres individuales
	' la idea es esta:
	' dentro de comentario --> 254 si es // y 255 si es /* */
	' final de linea con ";" --> 252
	' lineas que llevan un # --> 253
	' CR+LF --> dejo solo un 0D y lo cambio por 250 (me ahorro el 0A de momento)

	Print "Buscando comentarios, comillas y directivas #"
	
	
	' localizo los comentarios // y /* */
	' NOTA: como ya hemos pasado por un proceso anterior, ahora son del tipo FB, es decir '' y /' '/ 
	' es lo primero a hacer, por que de ese modo, el resto de busquedas es mas rapido, al no tratar los comentarios y saltarlos


	' tcpp es la cadena principal, donde va el codigo fuente CPP
	' tdat el auxiliar que almacena los comentarios, parentesis y llaves
	' tcar la longitud total del fichero (longitud de tcpp y tdat)
	
	For ff=1 To tcar ' longitud total de la cadena
		ss=Mid(tcpp,ff,1)
		escrlf=Asc(Mid(tdat,ff,1)) ' cojo el estado de CRLF marcado cuando hemos leido el fichero entero
			
		' aqui miramos si debemos dejar de comentar esa linea o lineas
		' excepto el caso /* */ por que se continua a pesar del CRLF
		If ss=Chr(CRLF) Then ' mi indicador de CR+LF
			' los #DEFINE pueden ser de varias lineas, separadas por "\", asi que, solo paro al acabar todas
			' si es tipo_grupo=REM2 (/* */) NO actuo aqui, por que es un grupo multilinea
			If tipo_grupo=DEFINES And leetexto(ff-1,1,0)<>"\" Then tipo_grupo=0 ' si hay "\" es que sigue en la sig. linea
			If tipo_grupo=REM1     Then tipo_grupo=0 ' solo si es del tipo //
			If tipo_grupo=COMILLAS Then tipo_grupo=0 ' por si acaso!!
		End If	
		
		If escrlf=0 Then Mid(tdat,ff,1)=Chr(tipo_grupo) ' activamos indicador de grupo (si es CRLF no se toca)
		
		
		
		' inicio de comentarios /*  o unitarios //
		If ss="/" And tipo_grupo<>COMILLAS Then ' no actua si son COMILLAS, pero si en DEFINE o REM1/2
			sr=Mid(tcpp,ff+1,1)
			If sr="*" And tipo_grupo<>REM1 Then ' tipo /*, y NO si ya hemos entrado en //
				tipo_grupo=REM2 
				Mid(tdat,ff+0,1)=Chr(REM2)
				Mid(tdat,ff+1,1)=Chr(REM2)
				ff+=1
			EndIf
			If sr="/" And tipo_grupo<>REM2 Then ' tipo //, y NO si ya hemos entrado en tipo /* */
				tipo_grupo=REM1 
				Mid(tdat,ff+0,1)=Chr(REM1)
				Mid(tdat,ff+1,1)=Chr(REM1)
				ff+=1
			EndIf
			sr="" ' borramos, para no crear errores al salir
		EndIf
		' final de comentarios tipo */
		If ss="*" And tipo_grupo=REM2 Then ' no actua si NO se ha marcado antes como REM2 (tipo /* */)
			sr=Mid(tcpp,ff+1,1)
			If sr="/" Then 
				Mid(tdat,ff+0,1)=Chr(REM2) ' final de comentarios tipo '/ (antes */)
				Mid(tdat,ff+1,1)=Chr(REM2)
				tipo_grupo=0 ' en este caso de comentarios "por lineas", dejamos de "comentar" aqui
				ff+=1
			EndIf
		EndIf
		
		
		
		' directivas
		If ss="#" And tipo_grupo<COMILLAS Then ' no actua si ya es COMILLAS, REM1 o REM2
			tipo_grupo=DEFINES ' directivas tipo #
			Mid(tdat,ff,1)=Chr(DEFINES) ' 253 en el caracter #
			'cambiatexto(ff,"'") ' borro el # (nota: he decidido dejarlo tal cual, por que normalmente son utiles)

			' en los #INCLUDE cambio '' o <> por "" y cambiar .h o .hxx por .bi
			If lcase(Mid(tcpp,ff+1,7))="include" Then
				For hh=0 To 100
					sr=Mid(tcpp,ff+hh,1)
					If Asc(sr)=CRLF Then Exit For ' si salgo de la linea, acabo, para no meterme en linea ajena
					If sr="<" Then cambiatexto(ff+hh,Chr(34))
					If sr=">" Then cambiatexto(ff+hh,Chr(34))
					If sr="." Then aa=ff+hh ' guardo la posicion del "." de la extension
				Next
				If aa>0 Then
					sr=Mid(tcpp,aa,4)
					If InStr(sr,".hxx") Then cambiatexto(aa,".bi"+Chr(34)+" "):sr=""
					If InStr(sr,".hpp") Then cambiatexto(aa,".bi"+Chr(34)+" "):sr=""
					If InStr(sr,".h")   Then insertatexto(aa,1):cambiatexto(aa,".bi"+Chr(34))
				End If	
			End If
		EndIf


		' NOTA: encontre un caso JODIDO como este -->> strcspn(p, "\"\r\n\0");  IMPOSIBLE de arreglar.
		'		  al aparecer unas comillas dentro de las comillas, se descoloca TODO. Aqui, a JODERSE.......
		' solo para cosas entre comillas (")
		If ss=Chr(34) And tipo_grupo=COMILLAS And APOSTROFES=0 Then ' esta linea DEBE ir antes de la otra, para NO interferir
			tipo_grupo=0 ' como ya se ha marcado arriba, antes de llegar aqui, simplemete, lo anulo
		ElseIf ss=Chr(34) And tipo_grupo=0 Then 
			tipo_grupo=COMILLAS ' cosas entre comillas ""
			Mid(tdat,ff,1)=Chr(COMILLAS) ' lo marco
		EndIf


		' solo para cosas entre apostrofes (')
		If ss="'" And APOSTROFES=1 Then ' esta linea DEBE ir antes de la otra, para NO interferir
			cambiatexto(ff,Chr(34)) ' apostrofe pasa a comillas
			tipo_grupo=0 ' como ya se ha marcado arriba, antes de llegar aqui, simplemete, lo anulo
			APOSTROFES=0 ' fin, hemos encontrado el cierre
		ElseIf ss="'" And tipo_grupo=0 Then 
			cambiatexto(ff,Chr(34)) ' apostrofe pasa a comillas
			tipo_grupo=COMILLAS ' cosas entre comillas ""
			APOSTROFES=1
			Mid(tdat,ff,1)=Chr(COMILLAS) ' lo marco
		EndIf

		
	Next
	
	
	' Ahora, cambio los comentarios tipo C por tipo FB (o sea, /* */ por /' '/ y // por ''"
	' (debe ir aqui, al final, para no cambiar cosas dentro de defines o de comillas)
	For ff=1 To tcar
		sa=leetexto(ff,1,0)
		If sa="/" And tipo>=REM1 Then ff=comentarios(ff) ' solo en comentarios REM1 y REM2
	Next
	
	'For ff=1 To tcar
	'	Print Mid(tcpp,ff+1,1),Asc(Mid(tdat,ff+1,1)):sleep
	'Next
	
End Sub





Sub busca_llaves_parentesis_puntoycoma()
	Dim As Integer aa,bb,ff ' a=contador de llaves, b=contador de parentesis, f= para bucles
	
	'For Ff=1 To tcar
	'	Print Mid(tcpp,ff,1), Asc(Mid(tdat,ff,1)):sleep
	'Next
	
	Print "Buscando 'punto y coma', llaves y parentesis"
	
	' contador de llaves, parentesis y finales de linea ";"
	aa=0 ' contador de llaves de 1 a 255 (0= no hay)
	bb=0 ' parentesis, de 1 a 255 (0= no hay)
  	For ff=1 To tcar

		sa=leetexto(ff,1)
		If tipo<DEFINES Then ' solo si no esta dentro de un comentario
			If sa="{" Then 
				aa+=1
				If aa>254 Then pon_error("Error: hay mas de 255 llaves a tratar",ff)
			EndIf
			'''''''
			If aa>0 Then Mid(tlla,ff,1)=Chr(aa) ' marcamos llaves
			'''''''
			If sa="}" Then 
				Mid(tlla,ff,1)=Chr(aa)  ' final de llave, va restando
				aa-=1
				' si al restar llegamos a 0, eso es bueno, coinciden
				If aa<0 Then pon_error("Error: El contador de llaves no cuadra",ff)
			EndIf

			
			If sa="(" Then 
				bb+=1
				'''Print "ABRE  :";bb,leetexto(ff-10,60)
				If bb>254 Then pon_error("Error: hay mas de 255 parentesis a tratar",ff)
			EndIf
			'''''''
			If bb>0 Then Mid(tpar,ff,1)=Chr(bb) ' marcamos parentesis
			'''''''
			If sa=")" Then 
				Mid(tpar,ff,1)=Chr(bb)  ' final parentesis, va restando
				bb-=1
				'''Print "CIERRA:";bb+1,leetexto(ff-10,60)
				' si al restar llegamos a 0, eso es bueno, coinciden
				If bb<0 Then pon_error("Error: El contador de parentesis no cuadra",ff)
			EndIf

			
			' y aqui, lo evidente, el ";"
			If sa=";" Then Mid(tdat,ff,1)=Chr(PUNTOYCOMA) ' final de linea
		
		Else ' si estamos en un define, comentarios, comillas, etc
			
			If aa>0 Then Mid(tlla,ff,1)=Chr(aa) ' si estamos dentro de una llave, marcamos tambien los demas tipos
			
		EndIf

  	Next

End Sub

Sub cambia_corchetes()
	Dim As Integer ff
  	Print "Cambiamos [] por ()"
  	For ff=1 To tcar ' fichero completo
		sa=leetexto(ff,1) ' no actua dentro de comillas o comentarios, pero si en defines
		If sa="[" Then cambiatexto(ff,"(")
		If sa="]" Then 
			If leetexto(ff+1,1)="[" Then 
				cambiatexto(ff,", ") ' caso de ][ en el ejemplo tipico MATRZ[1][2]
			ElseIf leetexto(ff+1,2)=" [" Then 
				cambiatexto(ff," , ") ' caso de ] [ en el ejemplo tipico MATRZ[1] [2] (con un espacio en medio)
			Else
				cambiatexto(ff,")")
			EndIf
		EndIf
  	Next	
End Sub

Sub cambios_basicos()
	Dim As Integer ff
	
	
	' NOTA: esta parte, YA se ha tratado ANTES en "aisla_grupos()"
	' pero busco de nuevo, por que los DEFINE no los trato en la anterior
  	Print "Cambiando ' por ";Chr(34)
  	For ff=1 To tcar
		sa=leetexto(ff,1)
		If sa="'" And tipo<COMILLAS Then ' se permiten cambios en los DEFINE/INCLUDE, pero no en COMILLAS o REM1, REM2
			cambiatexto(ff,Chr(34))
		EndIf
  	Next

	
End Sub  	



Sub operadores()
	Dim As Integer ff,gg

  	' toca los operadores ++  -- && ||  ^  **  ==   >>  <<  &  !  |  ^=  |=  &=  !=  ::
  	' (el & y && van luego, son mas complicados por que se usan tambien como punteros, en basic @)
  	ff=0
  	Print "Cambiar ++  --  &&  ||  ^  **  ==   >>  <<  &  !  |  ^=  |=  &=  !=  ::"
  	While (1) ' hasta que F no alcance el totalde caracteres, seguimos, varia segun quitamos o ponemos
  		If ff>tcar Then Exit while
  		ff+=1
		sa=leetexto(ff,1)   ' caracter uno

		' si esta dentro de un comentario /* */ o // , o COMILLAS , no actua, pero los DEFINE se permite
		If tipo<COMILLAS Then
			
			sb=leetexto(ff+1,1) ' caracter dos   
			sc=sa+sb        ' suma de los dos para hacer mas comoda la busqueda
			
			' primero las de igualdad doble != ^= &= |=
			If sc="!=" Then cambiatexto(ff,"<>"): sa=""
			
			' punteros dobles, dejo solo un "*"
			If sc="**" Then cambiatexto(ff," *"): sa=""
			
			' OR, XOR, AND
			If sc="|=" Then cambiatexto_espacios(ff,2,"Or=" ):sa=""
			If sc="^=" Then cambiatexto_espacios(ff,2,"Xor="):sa=""
			If sc="&=" Then cambiatexto_espacios(ff,2,"And="):sa=""

			' hexadecimales
			If sc="0x" Then cambiatexto(ff,"&h"): sa=""
							
			' dobles
			If sa="+" Then
				If sc="++" Then insertatexto(ff,2):cambiatexto(ff,"+=1 "):sa=""
			EndIf
			If sa="-" Then
				If sc="--" Then insertatexto(ff,2):cambiatexto(ff,"-=1 "):sa=""
				' nota: es mejor dejar "->" por que en FB implica acceso a PUNTERO, como en C
				'If sc="->" Then borratexto(ff,1):cambiatexto(ff,"."):sa="" ' casos STRUCT como en pepe->pepa, queda como pepe.pepa				
			EndIf
			If sa="<" Then
				If sc="<<" Then cambiatexto_espacios(ff,2,"Shl"):sa=""		
				' nota: es mejor dejar "<-" por que en FB implica acceso a PUNTERO, como en C	
				'If sc="<-" Then borratexto(ff,1):cambiatexto(ff,"."):sa="" ' casos STRUCT como en pepe<-pepa, queda como pepe.pepa	
			EndIf
			If sa=">" Then
				If sc=">>" Then cambiatexto_espacios(ff,2,"Shr"):sa=""	
			EndIf
			If sa="=" Then
				If sc="==" Then borratexto(ff,1):sa="" ' dejo un solo "="				
			EndIf


			' unitarios con posibilidad de dobles		
			If sa="|" Then
				If sc="||" Then 
					cambiatexto_espacios(ff,2,"Or"):sa=""
				Else
					' un solo | 
					cambiatexto_espacios(ff,1,"Or"):sa=""
				EndIf
			EndIf
			
			If sa=":" Then
				If sc="::" Then 
					cambiatexto(ff,"__"):sa="" ' el caso :: puede ser un objeto o una estructura, lo mejor, juntar la variable
				Else ' un solo :
					' hago un par de cambios cutres, solo para que den la nota en el fuente final
					
					' si solo es un ":" podria ser el tipico ( a ? 1 : 0 ), que en FB es IIF(a,1,0)
					' antes, debo ver si hay un "?" delante, para no confundirlo con un SWITCH (CASE/ENDCASE)
					For gg=ff To ff-100 Step -1 ' espero que con 100 sea suficiente)
						sd=leetexto(gg,1)
						' `=?  y ¨=:
						If sd="?" And tipo<CRLF Then cambiatexto(gg,"Ñ"):cambiatexto(ff,"ñ"):Exit For
					Next	

					' ANULADO AL 110% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
					' OLVIDARSE DEL TEMA "IIF"
					' hay docenas de casos, y es super dificil hacerlo. ejemplos dificiles REALES
					' return a?1:2; --> ni hay =, ni parentesis para centrarse 
					' a=a?1:2;  
					' a=a?a?2:2:2;  especialemente dificil dos juntos sin parentesis
					' if(a?1:0) pepe;  
					' tratar de buscar una logica es dificil, no hay guias de ; o () o = en los que basarse.
				EndIf
			EndIf
			
			If sa="!" Then ' ! con o sin parentesis seguido	
				'insertatexto(ff,1)
				cambiatexto_espacios(ff,1,"Not"):sa=""	
				' aqui tenemos un problema dificil: en el caso de un "!(" deberiamos poner "=0" tras el final... pero es muy dificil
				' por ejemplo --> if (a!=2 && !((a*2)+m)) ..... deberia ser algo asi --> if (a<>2 and ((a*2)+m)=0) ....
				' no lo trato, por lo complicado, es mejor arreglar a mano. 
			EndIf
			
			
			If sa="^" Then
				cambiatexto_espacios(ff,1,"Xor"):sa=""			
			EndIf
			
			
			' varias
			If sa="~" Then 
				insertatexto(ff,4):cambiatexto(ff," INV("):sa="" ' complemento en C++ del tipo '~' , lo mas parecido es -> -1-1*xx
			EndIf
			
			' este caso es SOLO para los tipicos DEFINE multilineas
			If sa="\" Then ' concatenacion de lineas, en basic es "_"
				insertatexto(ff,1):cambiatexto(ff," _"):sa=""
			End If
		
		
			' en el caso del & la cosa es mas complicada, por el tema de los punteros
			If sa="&" Then
				If sc="&&" Then cambiatexto_espacios(ff,2,"And"):sa="":sc=""
				If sc="& " Then cambiatexto_espacios(ff,2,"And"):sa="":sc=""
				If sc="&(" Then cambiatexto_espacios(ff,1,"And"):sa="":sc="" ' aqui solo indico 1 car. a cambiar, por que el "(" lo dejo
				' en caso de punteros, tratamos de ver si es o no un puntero (nota, solo tras filtrar los casos anteriores!!!)
				' es arriesgado y muy simple, pero puede valer: un puntero en C es del tipo &pepe o &_pepe
				' nunca esta el & separado, por que entonces seria AND. por eso, si tras el & hay una letra, asumo puntero 
				If esletra(Right(sc,1))=1 Or esletra(Right(sc,1))=2 Then cambiatexto(ff,"@"):sa="":sc="" ' y meto este en su lugar
				' si no se hace nada en esta anterior linea, miramos si es un & solitario, pero pegado, algo asi como "a&1"
				If Left(sc,1)="&" Then cambiatexto_espacios(ff,1,"And"):sa="":sc=""
			EndIf
			
		EndIf
  	Wend
  	
End Sub



Sub cambia_puntoycoma()
	Dim As Integer ff,gg
	Dim As String sa
	
	' cambio los ";" por nada en casos finales, y por ":" en casos de continuacion
	Print "Sustituyendo ';' por ':'"
	For ff=1 To tcar
		sa=leetexto(ff,1)
		If sa=";" And tipo=PUNTOYCOMA Then ' doble verificacion, por si acaso, mejor que sobre
			' encontrado un ;, analizo si es final o lleva una instruccion a continuacion
			cambiatexto(ff,":") ' lo cambia por un ":", provisional, porque luego, quizas tenga que borrarlo
			For gg=ff+1 To ff+300 ' solo miro 300 por delante, para no hacerlo muy largo, lo normal es que queden dentro del margen
				sa=leetexto(gg,1)
				' cambio el ":" que he metido antes por un " " para no alterar las columnas
				If tipo=PUNTOYCOMA Then Exit For ' si aparece "otro" ';' se sale, por que los ":" deben quedarse
				If tipo>=DEFINES Then cambiatexto(ff," "):Exit For ' si le sigue un comentario
				If Asc(sa)=CRLF  Then cambiatexto(ff," "):Exit For ' si es la unica linea
				If sa>" " Then Exit For ' cualquier otro caracter, se admite
			Next
		EndIf
	Next
	
End Sub



Sub quita_dospuntos()
	Dim As Integer ff
	
	''''''''''''''
	' quito los ":" sobrantes en los CASE/SWITCH
	Print "Eliminando ':'"
	ff=1
	While 1
		sa=leetexto(ff,1)
		'sb=leetexto(ff+1,1) ' para comprobar si son "::" y no tocarlos
		'If sa=":" And sb=":" Then End:Exit Sub ' "::" salimos, es subrutina CLASS, se trata aparte
		If tipo<DEFINES And sa=":" Then
			' en el caso del case (jeje), puede aparecer un "CASE 12: CASE 13". si quito el ":", queda mal, queda 
			' "CASE 12 CASE 13" y luego cuesta arreglar a mano, por eso, lo trato antes y lo paso a "CASE 12, 13"
			If (leetexto(ff,6)=": case") Or (leetexto(ff,5)=":case") Then
				borratexto(ff+1,5) ' quito el "_CASE" (espacio incluido, por que seguido viene otro, no hace falta dos
				cambiatexto(ff,",") ' meto la ","
			Else
				cambiatexto(ff," ")
			EndIf
		EndIf
		ff+=1
		If ff>tcar Then Exit while
	Wend
End Sub




' elimina palabas clave engorrosas, como LOCAL, PRIVATE, PUBLIC, EXTERN, STATIC, VOID, etc
engorros:
Data "local","private","public","extern","static","const","void","register","volatile","fin"
Sub elimina_engorros()
	Restore engorros
	
	Print "Eliminando cosas propias del C que en FB no sirven"
	
	Dim As Integer aa
	Dim As String sa,sb,sc
	
	sc=""
	While sc<>"fin"
		Read sc
		aa=1
		While 1
			sb=LCase(leetexto(aa,Len(sc)))
			If sb=sc And tipo<CRLF Then
				sb=leetexto(aa-1,Len(sc)+2)
				If (esletra(Left (sb,1))=0) and (esletra(right(sb,1))=0) Then  ' anterior y posterior NO son letra
					If Right(sb,1)=" " Then sb=" " Else sb=""' si hay un espacio seguido, lo quito tambien
					borratexto(aa,Len(sc+sb)) ' si es palabra engorro, la borro (solo si NO es TYPEDEF)
				End If
			EndIf
			aa+=1
			If aa>(tcar-Len(sc)) Then Exit While ' fin de la busqueda, a por otra
		Wend
	Wend
	
	
End Sub
