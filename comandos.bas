
'  no he llegado a emplear esto, pero lo dejo por si acaso, que me costo lo suyo.
'Function escomando(ByVal sa As String) As Integer
'	If sa="if" Then Return 1
'	If sa="else" Then Return 2
'	If sa="for" Then Return 3
'	If sa="switch" Then Return 4
'	If sa="case" Then Return 5
'	If sa="breal" Then Return 6
'	If sa="default" Then Return 7
'	If sa="return" Then Return 8
'	If sa="goto" Then Return 9
'	If sa="continue" Then Return 10
'	If sa="do" Then Return 11
'	If sa="while" Then Return 12
'	If sa="struct" Then Return 13
'	If sa="typedef" Then Return 14
'	If sa="enum" Then Return 15
'	''
'	If sa="==" Then Return 16
'	If sa="!=" Then Return 17
'	If sa=">=" Then Return 18
'	If sa="<=" Then Return 19
'	If sa="||" Then Return 20
'	If sa="&&" Then Return 21
'	If sa="->" Then Return 22
'	If sa="<-" Then Return 23
'	If sa=">>" Then Return 24
'	If sa="<<" Then Return 25
'	If sa="::" Then Return 26
'	If sa="" Then Return 27
'	If sa="~" Then Return 28
'	If sa="*=" Then Return 29
'	If sa="/=" Then Return 30
'	If sa="%=" Then Return 31
'	If sa="+=" Then Return 32
'	If sa="-=" Then Return 33
'	If sa=">>=" Then Return 34
'	If sa="<<=" Then Return 35
'	If sa="&=" Then Return 36
'	If sa="^=" Then Return 37
'	If sa="|=" Then Return 38
'	''
'	If sa="void" Then Return 40
'	If sa="static" Then Return 41
'	If sa="const" Then Return 42
'	If sa="extern" Then Return 43
'	If sa="unsigned" Then Return 44
'	If sa="signed" Then Return 45
'	If sa="char" Then Return 46
'	If sa="short" Then Return 47
'	If sa="int" Then Return 48
'	If sa="long" Then Return 49
'	If sa="wchar_t" Then Return 50
'	If sa="float" Then Return 51
'	If sa="double" Then Return 52
'	If sa="bool" Then Return 53
'	If sa="true" Then Return 54
'	If sa="false" Then Return 55
'	''
'	If sa="asm" Then Return 57
'	If sa="new" Then Return 58
'	If sa="this" Then Return 59
'	If sa="auto" Then Return 60
'	If sa="operator" Then Return 61
'	If sa="throw" Then Return 62
'	If sa="explicit" Then Return 63
'	If sa="private" Then Return 64
'	If sa="protected" Then Return 65
'	If sa="export" Then Return 66
'	If sa="try" Then Return 67
'	If sa="public" Then Return 68
'	If sa="catch" Then Return 69
'	If sa="register" Then Return 70
'	If sa="typeid" Then Return 71
'	If sa="reinterpret_cast" Then Return 72
'	If sa="typename" Then Return 73
'	If sa="class" Then Return 74
'	If sa="union" Then Return 75
'	If sa="friend" Then Return 76
'	If sa="const_cast" Then Return 77
'	If sa="using" Then Return 78
'	If sa="sizeof" Then Return 79
'	If sa="virtual" Then Return 80
'	If sa="inline" Then Return 81
'	If sa="delete" Then Return 82
'	If sa="static_cast" Then Return 83
'	If sa="volatile" Then Return 84
'	If sa="wchar_t" Then Return 85
'	If sa="mutable" Then Return 86
'	If sa="dynamic_cast" Then Return 87
'	If sa="namespace" Then Return 88
'	If sa="template" Then Return 89
'	''
'	If sa="ifdef" Then Return 90	
'	If sa="endif" Then Return 91	
'	If sa="include" Then Return 92	
'	If sa="define" Then Return 93
'	''
'	If sa="TRUE" Then Return 54
'	If sa="FALSE" Then Return 55	
'
'
'	Return 0
'End Function






' rutina comun a IF-FOR-WHILE 
' se encarga de localizar todo el grupo entre () tras el IF-FOR-WHILE
' y seguido busca el grupo de llaves que contiene la instruccion
' o en su defecto, si no hay llaves, el ";" que indica el final del comando
' devuelve el final del ";" o de la llave "}" final y la columna donde ha empezado el IF-FOR-WHILE para poner a
' la misma altura, el ENDIF-NEXT-WEND necesarios
' se aprovecha aqui, para juntar el grupo entre () en caso de ir en varias lineas, para que quede solo en una seguida
' aqui, las variables globales que se devuelven al acabar:
' inicio_comando: seria la columna donde ha empezado el comando, para poner a esa altura el ENDIF-NEXT-WEND
' fin_parentesis: seria la posicion tras el grupo () donde colocar el THEN en caso de IF, o un CRLF en caso de FOR, WHILE
' fin_llave: la ultima llave del grupo>1 (el grupo 1 siempre es la propia rutina) o "0" si no hay llave
' fin_puntoycoma: la posicion del ; si no hay llaves, o "0" si hay llaves
Dim Shared As Integer inicio_comando,inicio_parentesis,fin_parentesis,fin_llave,fin_puntoycoma
Sub localiza_inicio_fin(zona As Integer)
	Dim As Integer c,e,f,h

	' entrada "zona" es el primer caracter del IF-FOR-WHILE

	' obtengo aproximadamente la columna donde esta el IF-FOR-WHILE, para poner el ENDIF-NEXT-WEND a la par
	h=0 ' conteo de columnas
	c=0
	For f=zona To zona-100 Step -1 ' solo busco en 100, HACIA ATRAS, para no eternizar el bucle
		sa=leetexto(f,1) ' buscamos un CR(0D) anterior, la cosa mas simple
		If sa=Chr(&h09) Then h+=2 ' tabulacion FB=3 caracteres, pongo 2, que sumado a "TAB=9", son 3
		If sa=Chr(CRLF) Then h=h+(zona-f)-1:Exit for ' h=columna aprox
	Next
	' ahora, viene lo logico, y es que SIEMPRE hay un "(...)" tras cada comando tipo IF-FOR-WHILE
	' por eso, la LOGICA dice que si encuentro el final del parentesis ")" seguido, puedo meter el ENDIF-NEXT-WEND
	e=zona
	While 1 ' busco inicio de "("
		If e>tcar Then pon_error("Error de parentesis inicial en IF-THEN-WHILE",e)
		e+=1
		sd=leetexto(e,1)
		If sd="(" And nparentesis>0 Then f=e:c=nparentesis:Exit While ' encontrado inicio: f=posicion, c=valor_parentesis
	Wend
	While 1 ' busco final de ")", que coincida con el inicial en numero, almacenado en "c"
		If e>tcar Then pon_error("Error de parentesis final en IF-THEN-WHILE",e)
		e+=1
		sd=leetexto(e,1) 
		' aqui aprovecho a eliminar CRLF para juntar las lineas separadas del grupo (), para que quede todo en una sola
		If sd=Chr(CRLF) Then cambiatexto(e," ")
		If sd=")" And nparentesis=c Then Exit While ' coinciden entrada y salida, salimos con e=final de parnetesis					
	Wend
	
	' aqui tenemos:
	' h=columna inicio comando
	' f=incio (
	' e=final )
	inicio_comando=h
	inicio_parentesis=f
	fin_parentesis=e
	'Print "entre ():";leetexto(f,e-f+1)
	
	' pasamos a buscar final de llave } o en su defecto, el ";"  del final del comando solitario, si no hay {}
	' seguimos desde la variable fin_parentesis
	h=nllave ' guardo en grupo de {} en el que estamos, que como minimo, es el 1
	e=fin_parentesis
	f=0
	While 1
		If e>tcar Then pon_error("Error buscando fin llave en IF-THEN-WHILE",e)
		e+=1
		sd=leetexto(e,1)
		
		' si aparece antes una letra que una llave { es posible que solo sea una linea
		If esletra(sd) Then 
			If f=0 Then ' no hay llave aun, por lo tanto, buscamos ;
				While 1
					If e>tcar Then pon_error("Error buscando fin llave en IF-THEN-WHILE",e)
					e+=1
					sd=leetexto(e,1)	
					If sd=";" And nllave=h And nparentesis=0 Then 
						' salimos, hemos encontrado un ; antes que una {
						fin_llave=0 ' no hay llave
						fin_puntoycoma=e ' pero si posicion de punto y coma
						Exit Sub
					EndIf
				Wend
			EndIf
		EndIf
		
		If sd="}" And nllave=f Then 
			' salimos, hemos encontrado un ; antes que una {
			fin_llave=e ' posicion de llave final
			fin_puntoycoma=0 ' no hay ";"		
			Exit sub	
		EndIf
		If sd="{" And f=0 Then 
			If nllave>h And nparentesis=0 Then f=nllave ' ya tenemos llave incicial, ahora, a buscar su pareja
		EndIf
	Wend
	
	'fin_llave=0
	'fin_puntoycoma=0
	'sleep
End Sub





' trata bucles FOR
Sub trata_for()
	Dim As Integer aa,bb,cc,dd,ee,ff,gg
	Dim As String sa,sb

	Dim R As Integer 'contador principal

	Print "Tratamiento 'FOR / NEXT'"
		
  	R=0 
  	While 1
  		If (R>tcar) Then exit while ' hasta que no alcance el total de caracteres
  		R+=1

		sa=leetexto(R,1)   ' caracter uno
	
		' si esta dentro de un comentario o zona prohibida, no actua
		If tipo<DEFINES And sa="f" Then 
			gg=R ' para no tocar el bucle principal
			sc=leetexto(R,3) ' cogemos tres de golpe 
			
			
			' buscamos un FOR (en C solo es posible minusculas)
			If sc="for" Then 
				' compruebo si es un FOR realmente, y no algo como FORA o afor, o combinaciones de esas
				' 97='a', 122='z', ademas, miro tambien por un "_", que puede existir "for_pepe" y la liamos (o "_for")
				bb=0
				sa=LCase(leetexto(gg-1,1)) ' caracter anterior
				If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then bb=1 ' no es un FOR real
				
				sa=LCase(leetexto(gg+3,1)) ' caracter posterior +3
				If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then bb=1 ' no es un FOR real
				

				
				''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
				
				' si llegamos aqui, tenemos FOR real
				' miramos la columna aprox. en la que empieza el FOR, para meter un NEXT en la misma columna al final
				If bb=0 Then 
					dd=0
					For ff=gg To gg-100 Step -1 ' solo busco en 100, para no eternizar el bucle
						' obtengo aproximadamente la columna donde esta el comando
						' buscando un CR(0D) anterior, la cosa mas simple
						sa=leetexto(ff,1)
						If sa=Chr(&h09) Then dd+=2 ' tabulacion, 2 caracteres
						'si hay un retorno (mi identificador de CRLF)
						If sa=Chr(CRLF) Then dd=dd+(gg-ff-2):Exit for ' D=columna aprox, sin tener en cuenta las tabulaciones, solo posiciones
					Next
					
					' quito los parentesis de entrada y salida
					ee=0
					For ff=gg To gg+500 ' busco un monton, por que puede ser largo, o estar en dos lineas
						sa=leetexto(ff,1)
						If sa="(" And nparentesis=1 Then cambiatexto(ff," "):ee=ff ' me cargo el primero
						If sa=")" And nparentesis=1 Then cambiatexto(ff," "):Exit for ' me cargo el segundo y salgo, ya no hace falta mas
						'si hay un retorno (mi identificador de CRLF), me lo cargo tambien, para juntar lineas
						If sa=Chr(CRLF) Then cambiatexto(ff," ") 
					Next					
					
					
					' tratamos de reconstruir el bucle, algo MUY dificil
					'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
					aa=InStr(gg+0,tcpp,";") ' primer ; donde va el "To"
					ee=InStr(aa+1,tcpp,";") ' segundo ; donde va el STEP
					sa=Mid(tcpp,ee+1,ff-ee) ' cojo el ultimo grupo, el que seria STEP
					sb=""
					If InStr(sa,"+=1") Then sb="" ' y construyo el STEP , logicamente, "STEP 1" sobra....
					If InStr(sa,"-=1") Then sb="Step -1"
					borratexto(ee+1,Len(sa)) ' borro este grupo, ya no sirve para nada
					'''''''''''''''''''''''''''''''''''''''''''''''''''''
					aa=InStr(gg,tcpp,";") ' primer ; donde va el "To"
					cambiatexto (aa," ") ' quito el primer ;
					insertatexto(aa+1,4)
					cambiatexto (aa," To ") ' añadimos un "To"
					''''''''''''''''''''''''''''''''''''''''''''''''''''''
					ee=InStr(aa,tcpp,";") ' segundo ; donde va el STEP
					cambiatexto (ee," ") ' quito el segundo ;
					insertatexto(ee+1,6)
					cambiatexto (ee,sb) ' añadimos el STEP calculado arriba (o vacio, si corresponde)
					''''''''''''''''''''''''''''''''''''''''''''''''''''''


					''''''''''''''''''''''''''''''''''''''''''''''''''''''				
					' toca contar llaves para quitarlas y poner el NEXT
					 'uso la posicion de inicio de la variable "ee" usada justo antes, y sigo desde ahi
					' busco la primera
					' NOTA: he visto ejemplos de FOR seguido de FOR SIN llaves, y ese tipo NO SOY CAPAZ de arreglar. eso, a mano
					While 1
						If ee>tcar Then pon_error( "Error '1' de llaves en casos FOR",ee)
						ee+=1
						sd=leetexto(ee,1)
						'Print ee,sd
						'sleep
						''
						If sd=";" And tipo=PUNTOYCOMA Then
							' hemos encontrado ANTES de la "{" un ";", eso indica (posiblemente) una UNICA linea
							' en este caso, NO buscamos llaves "{}", sino que usamos la pos. del ";" para meter ahi el final
							insertatexto(ee,4+1+dd) ' hago hueco para meter el valor DD (columna), mas un CRLF y el "WEND"
							cambiatexto(ee, Chr(CRLF)+Space(dd)+"Next"+Chr(CRLF) )' el primer CRLF "pisa" la llave "}"						
							ee=0 ' indicamos que NO hay llaves
							Exit While
						EndIf
						''
						If sd="{" And nllave>1 Then ' las llaves de valor 1 son las principales, las de la sub., por eso, busco desde la 1
							cambiatexto(ee," ") ' quitamos la llave {
							cc=nllave ' guardamos el valor de la llave, que deberia ser mayor que 1, para luego buscar su pareja
							Exit While
						End if	
					Wend
					ff=ee ' guardo la posicion de la llave inicial
					' busco la segunda, la del final
					If ee>0 Then ' si ee=0 es que no hay llaves (caso de linea unica, ver arriba, al buscar la primera llave
						While 1
							If ee>tcar Then pon_error( "Error '2' de llaves en casos FOR",ee)
							ee+=1
							sd=leetexto(ee,1)
							If sd="}" And nllave=cc Then ' coinciden entrada y salida
								insertatexto(ee,4+1+dd) ' hago hueco para meter el valor DD (columna), mas un CRLF y el "NEXT"
								cambiatexto(ee, Chr(CRLF)+Space(dd)+"Next"+Chr(CRLF) )' el primer CRLF "pisa" la llave "}"						
								Exit While
							End If
						Wend
					End If
					
				End If ' fin de cambios dentro del "for"
				
				
				'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
				
			EndIf ' fin de "hemos encontrado for"


		End If	' seguimos buscando
  	Wend

End Sub



' trata CASE/SWITCH
Sub trata_case()
	Dim As Integer aa,bb,cc,dd,ee,ff,gg
		
	Dim R As Integer 'contador principal

	Print "Tratamiento 'SWITCH / CASE'"
		
  	R=0 
  	While 1
  		If (R>tcar) Then exit while ' hasta que no alcance el total de caracteres
  		R+=1

		sa=leetexto(R,1)   ' caracter uno
	
		' si esta dentro de un comentario o zona prohibida, no actua
		If tipo<DEFINES And sa="s" Then 
			gg=R ' para no tocar el bucle principal
			sc=leetexto(R,6) ' cogemos seis de golpe 
			
			
			' buscamos un SWITCH (en C solo es posible minusculas)
			If sc="switch" Then 
				' compruebo si es un FOR realmente, y no algo como SWITCHA o aSWITCH, o combinaciones de esas
				' 97='a', 122='z', o que lleve un "_" delante o detras
				bb=0
				sa=LCase(leetexto(gg-1,1)) ' caracter anterior
				If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then bb=1 ' no es un SWITCH real
				
				sa=LCase(leetexto(gg+6,1)) ' caracter posterior +6
				If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then bb=1 ' no es un SWITCH real
				
				
				
				''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
				' si llegamos aqui, tenemos SWITCH real
				' miramos la columna aprox. en la que empieza el SWITCH, para meter un END SELECT en la misma columna al final
				If bb=0 Then 
					dd=0
					For ff=gg To gg-50 Step -1 ' solo busco en 50, para no eternizar el bucle
						' obtengo aproximadamente la columna donde esta el comando
						' buscando un CR(0D) anterior, la cosa mas simple
						sa=leetexto(ff,1)
						If sa=Chr(&h09) Then dd+=2 ' tabulacion, 2 caracteres
						'si hay un retorno (mi identificador de CRLF)
						If sa=Chr(CRLF) Then dd=dd+(gg-ff-2):Exit for ' D=columna aprox, sin tener en cuenta las tabulaciones, solo posiciones
					Next
					
					' cambio el SWITCH por Select
					insertatexto(gg,6) ' hueco para el nuevo CASE
					''''''''''''''''''''''''''''''''''''''''''''''''''''''
					cambiatexto (gg,"Select Case ") ' meto SELECT CASE
					''''''''''''''''''''''''''''''''''''''''''''''''''''''

					' contamos llaves 
					ee=gg
					' busco la primera
					While 1
						If ee>tcar Then pon_error("Error '1' de llaves en SWITCH",gg)
						ee+=1
						sd=leetexto(ee,1)
						If sd="{" And nllave>1 Then ' las llaves de valor 1 son las principales, las de la sub., por eso, busco desde la 1
							cambiatexto(ee," ") ' quitamos la llave {
							cc=nllave ' guardamos el valor de la llave, que deberia ser mayor que 1, para luego buscar su pareja
							Exit While
						End if	
					Wend
					ff=ee ' guardo la posicion de la llave inicial
					' busco la segunda, la del final
					' aprovechando que "viajamos" dentro del grupo de llaves, miro si hay BREAK y el posible DEFAULT
					While 1
						If ee>tcar Then pon_error("Error '2' de llaves en SWITCH",gg)
						ee+=1
						sd=leetexto(ee,1)
						If sd="}" And nllave=cc Then ' coinciden entra y salida
							insertatexto(ee,10+1+dd) ' hago hueco para meter el valor DD (columna), mas un CRLF y el "END SELECT"
							cambiatexto(ee, Chr(CRLF)+Space(dd)+"End Select"+Chr(CRLF) )' el primer CRLF "pisa" la llave "}"						
							Exit While
						End if	
						
						' buscamos BREAK
						sd=leetexto(ee,5) ' se busca un "break" solo, y que no lleve "_" delante o detras (como "_break")
						If sd="break" Then 
							bb=0
							sa=LCase(leetexto(ee-1,1)) ' caracter anterior
							If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then bb=1 
							sa=LCase(leetexto(ee+5,1)) ' caracter posterior
							If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then bb=1 
							If bb=0 Then ' si, es un "break"
								cambiatexto(ee,"'") ' asi de simple, mantengo el resto, como "reak", para luego identicarlo en FB
							End If
						EndIf
						
						' buscamos DEFAULT
						sd=leetexto(ee,7) ' se busca un "default" solo, y que no lleve "_" delante o detras (como "default_")
						If sd="default" Then 
							bb=0
							sa=LCase(leetexto(ee-1,1)) ' caracter anterior
							If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then bb=1 
							sa=LCase(leetexto(ee+7,1)) ' caracter posterior
							If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then bb=1 
							If bb=0 Then ' si, es un "default"
								insertatexto(ee,2) 'necesito dos huecos mas
								cambiatexto(ee,"case else")
							End If
						EndIf
						
					Wend

				End If ' fin de cambios dentro del "switch"
				''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
				
				
				
			EndIf ' fin de "hemos encontrado switch"

		End If	' seguimos buscando
		
  	Wend
	
End Sub




' trata bucles WHILE, DO
Sub trata_while()
	Dim As Integer aa,bb,cc,dd,ee,ff,gg
		
	Dim R As Integer 'contador principal

	Print "Tratamiento 'WHILE'"
		
  	R=0 
  	While 1
  		If (R>tcar) Then exit while ' hasta que no alcance el total de caracteres
  		R+=1

		sa=leetexto(R,1)   ' caracter uno
	
		' si esta dentro de un comentario o zona prohibida, no actua
		If tipo<DEFINES And sa="w" Then 
			gg=R ' para no tocar el bucle principal
			sc=leetexto(R,5) ' cogemos cinco de golpe 
			
			
			' buscamos un SWITCH (en C solo es posible minusculas)
			If sc="while" Then 
				' compruebo si es un FOR realmente, y no algo como WHILAA o aWHILE, o combinaciones de esas
				' 97='a', 122='z', o que lleve "_" delante o detras, como  "_while_" o cosas asi
				bb=0
				sa=LCase(leetexto(gg-1,1)) ' caracter anterior
				If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then bb=1 ' no es un WHILE real
				
				sa=LCase(leetexto(gg+5,1)) ' caracter posterior +5
				If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then bb=1 ' no es un WHILE real
				
				
				
				''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
				' si llegamos aqui, tenemos WHILE real
				' miramos la columna aprox. en la que empieza el SWITCH, para meter un END SELECT en la misma columna al final
				If bb=0 Then 
					dd=0
					For ff=gg To gg-50 Step -1 ' solo busco en 50, para no eternizar el bucle
						' obtengo aproximadamente la columna donde esta el comando
						' buscando un CR(0D) anterior, la cosa mas simple
						sa=leetexto(ff,1)
						If sa=Chr(&h09) Then dd+=2 ' tabulacion, 2 caracteres
						'si hay un retorno (mi identificador de CRLF)
						If sa=Chr(CRLF) Then dd=dd+(gg-ff-2):Exit for ' D=columna aprox, sin tener en cuenta las tabulaciones, solo posiciones
					Next
										
					' contamos llaves 
					ee=gg
					' busco la primera
					While 1
						If ee>tcar Then pon_error( "Error '1' de llaves en WHILE",gg)
						ee+=1
						sd=leetexto(ee,1)
						''
						If sd=";" And tipo=PUNTOYCOMA Then
							' hemos encontrado ANTES de la "{" un ";", eso indica (posiblemente) una UNICA linea
							' en este caso, NO buscamos llaves "{}", sino que usamos la pos. del ";" para meter ahi el final
							insertatexto(ee,9+1+dd) ' hago hueco para meter el valor DD (columna), mas un CRLF y el "WEND"
							cambiatexto(ee, Chr(CRLF)+Space(dd)+"Wend"+Chr(CRLF) )' el primer CRLF "pisa" la llave "}"						
							ee=0 ' indicamos que NO hay llaves
							Exit While
						EndIf
						''
						If sd="{" And nllave>1 Then ' las llaves de valor 1 son las principales, las de la sub., por eso, busco desde la 1
							cambiatexto(ee," ") ' quitamos la llave {
							cc=nllave ' guardamos el valor de la llave, que deberia ser mayor que 1, para luego buscar su pareja
							Exit While
						End if	
					Wend
					ff=ee ' guardo la posicion de la llave inicial
					' busco la segunda, la del final
					If ee>0 Then ' si ee=0 es que no hay llaves (caso de linea unica, ver arriba, al buscar la primera llave
						While 1
							If ee>tcar Then pon_error( "Error '2' de llaves en WHILE",gg)
							ee+=1
							sd=leetexto(ee,1)
							If sd="}" And nllave=cc Then ' coinciden entra y salida
								insertatexto(ee,9+1+dd) ' hago hueco para meter el valor DD (columna), mas un CRLF y el "WEND"
								cambiatexto(ee, Chr(CRLF)+Space(dd)+"Wend"+Chr(CRLF) )' el primer CRLF "pisa" la llave "}"						
								Exit While
							End if							
						Wend
					End If

				End If ' fin de cambios dentro del "WHILE"
				''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
				
				
				
			EndIf ' fin de "hemos encontrado WHILE"

		End If	' seguimos buscando
		
  	Wend
	
End Sub




' trata comandos IF
Sub trata_if()
	Dim As Integer c,e,f
	Dim As Integer ee,ff,cc ' para buscar posiciones de llaves y parentesis
	Dim parentesis As Integer
	
	' RGB, bucles 
	Dim R As Integer ' principal, por todo el fichero
	Dim G As Integer ' secundario, para cada grupo encontrado en el IF
	Dim B As Integer ' auxilar, para dentro de cada mini-grupo, dentro del grupo G
		
	Dim tenemosIF   As Integer=0
	Dim tenemosELSE As Integer=0
	Dim ponerENDIF  As Integer=1 ' para ver cuantas lineas tiene el grupo IF y si solo es UNA, no se pone ENDIF
	Dim finalTHEN As Integer ' posicion del final del "Then" que pongamos
	Dim columnaENDIF As Integer ' posicion donde poner el ENDIF
	Dim tenemosELSEIF As Integer=0 ' solo para los ELSE IF
	Dim As String sd
	
	Print "Tratamiento 'IF / THEN'"

  	R=0 
  	While 1

  		If (R>tcar) Then exit while ' hasta que no alcance el total de caracteres
  		R+=1
  		
  		tenemosIF=0
  		tenemosELSE=0
  		ponerENDIF=1 ' por defecto, simepre se pone endif en "nueva" linea, pero si va todo seguido, se hace en la misma
  		
		sa=leetexto(R,1)   ' caracter uno

		' si esta dentro de un comentario o zona prohibida, no actua
		If tipo<DEFINES Then 
			G=R ' para no tocar el bucle principal
			sb=leetexto(G+1,1) ' caracter dos   
			sc=sa+sb           ' suma de los dos para hacer mas comoda la busqueda
			ponerENDIF=1 ' 1=pone END IF si es necesario
		
			
			' buscamos un IF (en C solo es posible minusculas)
			If sc="if" Then 
				' compruebo si es un IF realmente, y no algo como PIF o Rif o RIFA, combinaciones de esas
				' 97='a', 122='z', o que lleve "_" pegado a el, tipo "_if" o "if_pepe"
				B=0
				sa=LCase(leetexto(G-1,1)) ' caracter anterior
				If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then B=1' no es un IF real
				
				sa=LCase(leetexto(G+2,1)) ' caracter posterior +2
				If Asc(sa)>96 And Asc(sa)<123 Or sa="_" Then B=1 ' no es un IF real
				
				
				
				' ahora, si B=0, es que YA tenemos IF real...
				' contamos parentesis, para encontrar el ultimo y meter ahi un THEN
				If B=0 Then 
					columnaENDIF=0
					localiza_inicio_fin(G)
					insertatexto(fin_parentesis+1,6)
					''''''''''''''''''''''''''''''''''''''''''''''''''''''
					cambiatexto (fin_parentesis+1," Then ") ' añadimos un THEN
					''''''''''''''''''''''''''''''''''''''''''''''''''''''
					'micolor(amarillo):Print "Estudiando : ";Trim(leetexto(G,(parentesis-G+1))):micolor(blanco)
					''''''''''''''''''''''''''''''''''''''''''''''''''''''
					tenemosif=1 ' ya podemos seguir
					G=fin_parentesis+1+6 ' me pongo tras el " THEN "
					finalthen=G ' posicion final de THEN donde poder meter un CRLF al acabar, si es necesario
					If tenemosELSEIF Then 
						tenemosELSEIF=0
						insertatexto(G,1)
						cambiatexto(G-1,Chr(CRLF))
					EndIf
				End If
			EndIf
			
			
			'si hemos encontrado un If y metido un Then, podemos buscar el resto, como un ELSE
			If tenemosIF Then
				
				'''''''''''''''''''''''''''''''
				'' primer grupo antes del ELSE
				'''''''''''''''''''''''''''''''
				' miramos si hay llaves en el primer grupo, antes de un posible ELSE
				' si hay, las quita, si no hay, busca el final de la "supuestamente" unica linea
				B=G ' posicion temporal desde donde seguir
				While 1 
					sd=leetexto(B,1) ' cojo el tipo de caracter a tratar
					If tipo<DEFINES Then ' no dentro de comentarios
					   
						If tipo=CRLF Then ' encontrado retorno de linea antes que el ";"
							If ponerENDIF=1 Then ponerENDIF=2 ' si hay salto de linea tras el THEN, por narices necesitamos END IF
						EndIf
						
						If sd=";" And tipo=PUNTOYCOMA Then ' encontrado un ";" , doble comprobacion
							If ponerENDIF=1 Then
								ponerENDIF=0 ' si ";" viene antes de CRLF , esta todo en la misma linea, NO ponemos ENDIF
								'''''
								' ahora, para solucionar un caso tipo --> "IF (pp==1) {pp=2; IF (PP2=2) PP2=3; } (un IF seguido de 
								' otro en la misma linea los dos y con un ELSE tras el primero, CASI NADA)
									For F=B To B+300
										sd=leetexto(F+1,1)
										If tipo=55 Then ponerENDIF=3 ' =3 es caso especial dos IF seguidos, con tipo especial=55
									Next F
								'''''
							EndIf
							G=B ' actualizamos posicion
							Exit While ' y salgo en busca de un ELSE
						EndIf
						
						If sd="{" And nllave>1 Then ' encontrada una llave { mayor de 1 (la 1 es la principal, la del SUB)
							cambiatexto(B," ") ' quito la primera llave
							ee=nllave ' copia del nº de la llave inicio encontrada
							While 1
								sd=leetexto(B,1) ' ahora busco el final de las llaves
								If tipo=CRLF Then ' encontrado retorno de linea antes que  "}"
									If ponerENDIF=1 Then ponerENDIF=2 ' si hay salto de linea tras el THEN, por narices necesitamos END IF
								EndIf
								If sd="}" And nllave=ee Then ' encontrada llave final coincidente
									''''''
									' nota: para solucionar un caso especial de IF dentro de otro IF en la misma linea
									' he inventado un rollo, que si borro solo la llave final, pero pongo el TDAT=55
									' la siguiente pasada en busca de IF, lo trata como si estuviera todo junto
									' y no mete saltos ni else ni nada. YA VEREMOS SI ESTO HA FUNCIONADO
									  Mid(tdat,B,1)=Chr(55) ' al ser un caso especial, me invento un tipo especial, el 55, que espero no exista!!!!
									''''''
									cambiatexto(B," ") ' quito la ultima llave, pero SOLO quito "}", y pongo su TDAT=55
									G=B ' actualizamos posicion
									Exit While ' y salgo en busca de un ELSE  (nota, sale al siguiente EXIT WHILE de abajo)
								EndIf
								B+=1
								If B>tcar Then pon_error("Error 'A' en llaves 'IF'",R)
							Wend
							Exit While ' aqui solo llega, si salimos del anterior WHILE, y sale del TODO
						EndIf
						
					End If
					B+=1
					' no deberia NUNCA alcanzar el final, sino, algo no va. DEBE salir antes, tras cumplir algo de lo anterior
					If B>tcar Then pon_error("Error '1' en 'IF'",R)
				Wend


				''''''''''''''''''''''
				'' busqueda del ELSE
				''''''''''''''''''''''	
				B=G ' posicion temporal desde donde seguir
				While 1 
					sa=leetexto(B,1)
					If tipo<DEFINES Then ' NO dentro de comentarios o #DEFINES
						If sa>="a" And sa<="z" Then
							sb=leetexto(B,4)
							If sb="else" Then 
								tenemosELSE=1:G=B+4
								tenemosELSEIF=0
								' miramos si ademas, es un ELSE IF (en cuyo caso "tenemoselse" vale 2)
								sb=leetexto(B,7)
								If sb="else if" Then
									'Color 2,0:Print "REVISA LOS 'ELSE IF' POR QUE NO SUELEN QUEDAR BIEN....":Color 7,0
									tenemosELSE=2:G=B+7
									tenemosELSEIF=B+4' guardo la posicion del "nuevo" IF que hay tras el ELSE
								EndIf
								Exit While
							Else
								' no actuamos, no hay ELSE
								tenemosELSE=0
								Exit While ' ha aparecido como primer caracter de la secuencia, otra cosa distinta a "else" 
							End If
						EndIf
					EndIf
					B+=1
					' no deberia NUNCA alcanzar el final, sino, algo no va. DEBE salir antes, tras cumplir algo de lo anterior
					If B>tcar Then pon_error("Error '2' en 'IF'",R)
				Wend			
				
				''''''''''''''''''''''
				'' segundo grupo tras el ELSE (puede ser "tenemoselse=1" o "tenemoselse=2"
				'' si es "1" es un ELSE final, si es "2" es un "ELSE IF" y hay que seguir
				''''''''''''''''''''''
				If tenemosELSE Then
					If ponerENDIF<>3 Then ponerENDIF=1 ' si hay ELSE, el ENDIF es necesario
					B=G ' posicion temporal desde donde seguir
					' añado un salto de linea tras el Else, por que suele pasar que se queda todo en la misma linea
					' es solo como seguridad, mejor que sobre a que falte. si luego sobra, se quita cuando se guarda
					' pero no lo hago si es un ELSE IF
					If tenemosELSE=1 Then
						insertatexto(G,8+columnaENDIF) ' ademas, sumo la tabulacion del THEN
						cambiatexto(G,Chr(CRLF)+Space(columnaENDIF))
					End If
					' si es un ELSE IF, no es else solitario, asi que, salgo sin tocar nada mas
					' de forma que el "nuevo" IF se trate desde cero como uno normal
					If tenemosELSE=2 And tenemosELSEIF>0 Then GoTo noesELSEaun
					'
					B+=1
					While 1 
						sd=leetexto(B,1) ' cojo el tipo de caracter a tratar
						If tipo=PUNTOYCOMA Then ' encontrado un ;
							'borratexto(B,1) ' quito el ; final(nota actualizacion: ahora dejo que lo haga la rutina, es mas seguro)
							G=B ' actualizamos posicion
							Exit While ' y salgo A poner el ENDIF
						EndIf
						If sd="{" And nllave>1 Then ' encontrada una llave { mayor de 1 (la 1 es la principal, la del SUB)
							cambiatexto(B," ") ' quito la primera llave
							ee=nllave ' guardo inicio de llave
							While 1
								sd=leetexto(B,1) ' ahora busco el final de las llaves
								If sd="}" And nllave=ee Then 
									cambiatexto(B," ") ' borro la llave final
									G=B ' actualizamos posicion
									Exit While ' y salgo a poner el ENDIF (nota, sale al siguiente EXIT WHILE de abajo)
								EndIf
								B+=1
								If B>tcar Then pon_error("Error 'B' en llaves 'IF' tras ELSE",R)
							Wend
							Exit While ' llega si sale del anterior WHILE
						EndIf
						B+=1
						' no deberia NUNCA alcanzar el final, sino, algo no va. DEBE salir antes, tras cumplir algo de lo anterior
						If B>tcar Then pon_error("Error '3' en 'IF'",R)
					Wend
				End If
				
				
				''''''''''''''''''''''
				' con los grupos localizados, pasamos a poner END IF
				'''''''''''''''''''''
				' nota: no habia ELSE y solo hemos bajado UNA linea (un solo CRLF) es un IF en una sola linea
				' en ese caso, el END IF sobra, por lo tanto, esta rutina no se hace
				'Print ponerENDIF:sleep
				If ponerENDIF=3 Then ponerENDIF=0:tenemosELSE=0 ' caso especial dos IF seguidos en la misma linea
				If ponerENDIF>0 Then 
					insertatexto(G,1+columnaENDIF+6+1) ' hueco para meter la columna aprox. donde esta el IF
					cambiatexto(G, Chr(CRLF)+Space(columnaENDIF)+"EndIf"+Chr(CRLF) )
					' añado un salto de linea tras el THEN, por que suele pasar que se queda todo en la misma linea
					' es solo como seguridad, mejor que sobre a que falte. si luego sobra, se quita cuando se guarda
					insertatexto(finalTHEN,1+columnaENDIF) ' ademas, la tabulacion del THEN
					cambiatexto(finalTHEN,Chr(CRLF)+Space(columnaENDIF))
				End If
				If ponerENDIF=0 And tenemosELSE=0 Then
					' caso especial, con la 'unica' linea tras el IF, en la segunda linea, al estilo:
					' if (a=1)
					'      printf("2");
					For F=finalTHEN To finalTHEN+300 ' con 300 suficiente
						' la varible 'finalTHEN' es el final del THEN, desde donde empiezo a buscar "posible" CRLF
						sb=leetexto(F,1)
						If tipo<DEFINES Then ' no en comentarios
							If sb=Chr(9)    Then borratexto(F,1):F-=1:sb="" ' borra las tabulaciones, por que ahora va todo seguido del THEN
							If sb=" "       Then borratexto(F,1):F-=1:sb="" ' borra los espacios delante, por que ahora va todo seguido del THEN
							If sb=Chr(CRLF) Then borratexto(F,1):F-=1:sb="" ' encontrado CRLF, se quita
							If sb>" " Then Exit For ' sale en cuanto encuentra algo distinto al espacio o el CRLF
						End If
					Next	
				End If
			End If ' fin bucle "tenemosif"
	   noesELSEaun:
		End If	' fin bucle de busqueda de "; {} y ELSE", al salir seguimos en la pos. R, justo tras el primer IF
  
  	Wend
  	
  	'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  	' ahora, busco los posibles "else if" y los dejo como el FB necesita, "ElseIf"
  	'''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
  	R=0 
  	While 1
  		If (R>tcar) Then exit while ' hasta que no alcance el total de caracteres
  		R+=1
		sa=leetexto(R,7)   ' caracter uno
		If tipo<DEFINES Then ' si esta dentro de un comentario o zona prohibida, no actua
			If sa="else if" Then cambiatexto(R,"ElseIf ")
		EndIf
  	Wend
  	
End Sub




' intenta distinguir instrucciones de variables
Sub instrucciones()
	Dim As Integer aa,bb,cc,dd,ee,ff,gg,rr
	Dim As String sa,sb,sc,sd,sf,params
	'Dim As Integer comando
	Dim As Integer inicio
	Dim As Integer longitud
	Dim As Integer hayigual
	Dim As Integer inillave ' es para contar las llaves inicio/fin y meter "end funcion" al cerrar la llave de valor 1
	Dim As Integer finllave ' es para contar las llaves inicio/fin y meter "end funcion" al cerrar la llave de valor 1
	Dim As Integer tipollave ' guardo si estamos en llaves o no, y que llave es
	'Dim As Integer hayparentesis
	'Dim As Integer haycorchete
	Dim As Integer fininstruccion
	Dim As String comandos(200)
	
	Dim R As Integer 'contador principal

	Print "Localizando Subrutinas o variables"
		
  	R=0 
  	sf=""
  	cc=0 ' contador de comandos

	inicio=0 ' inicio de la supuesta instruccion (por ejemplo, en "static void pepe(int a);" seria la "s" de "static"
	longitud=0 ' total de caracteres de la instruccion completa, hasta el "(" o "[" o ";", etc
	
  	While 1
  		R+=1
  		If (R>tcar) Then exit while ' hasta que no alcance el total de caracteres
		
		' punto y coma finaliza la busqueda:
		' puede ocurrir que el punto y coma sea el del fin de una gigantesca subrutina, lo que generaria
		' cientos de comandos hasta llegar a ese ";", por eso, mas adelante, se verifica si eso es cierto y se actua
		' la idea aqui, es recoger nada mas los comandos antes del ";" de definiciones del tipo que sean
		' como declaraciones de funciones o subrutinas o variables de cualquier tipo
		' ...... pero la subrutina como tal, no se trata aqui, solo su declaracion .......
		' lo mismo que las variables tipo ={} que suelen ser muy largas. estes se tratan despues.
		' aqui solo pretendo recoger lo de delante del ; en casos como:
		' static struct BinTree *AllocBinTree(int Entry, VoidPtr Data); // declaracion de funciona
		' int &pepe=1.1; // declaracion de variable
		sa=leetexto(R,1)   ' caracter a estudiar

		If tipo=PUNTOYCOMA And inicio>0 Then fininstruccion=1 ' si "inicio" es 0 no actua, por que no se ha leido nada aun
					
		' si esta dentro de un comentario o define, no actua
		If tipo<DEFINES Then 
			
			
			' si aparece un "=" vigilamos si aparece seguido un "{" , indicativo de variable que puede ser muy larga
			If sa="{" And hayigual Then
				' si aparece una llave, y antes de ella ha aparecido un = miramos si es una variable
				If R>hayigual Then 
					' solo si la llave viene despues del igual, osea, si R>hayigual
					fininstruccion=1 ' finalizamos, por que el resto, desde la llave hasta su final "};" no interesa
					' pero buscamos el ";" final, para luego seguir desde ahi
					'For ff=R To tcar
					'	sb=leetexto(ff,1)
					'	If nllave=0 Then R=ff:Exit For ' localizamos el final de la llave, que deberia ser la 1
					'Next
				EndIf
			EndIf
			If sa="=" Then hayigual=R 
			
			
			' estos elementos tambien finalizan, no interesa lo que viene tras ellos
			If sa="(" Then fininstruccion=2 ' posible subrutina, por que aparece un "(" al final
			If sa="[" Then fininstruccion=1 ' variable
			If sa="," Then fininstruccion=1 ' desconocido, puede ser variable, struct, sub...
			
			
			
			' si es ASCII lo tratamos			
			If esletra(sa) Then
				If nllave=1 Then sa="" ' NO miramos DENTRO de llaves {} valor 1, en este modulo, solo miro FUERA de {}
				If inicio=0 Then inicio=R':tipollave=nllave ' guardo el inicio y si estamos en llave o no
				sf=sf+sa ' sumo lo legible ASCII, los espacios y algunos caracteres hacen el salto a otro comando
			Else ' si no lo es, miramos lo que hemos conseguido hasta ese punto
				sf=Trim(sf) ' si hemos llegado aqui, es que ha aparecido un espacio o un caracter NO ASCII
				' si tenemos texto localizado, se guarda como posible comando
				If sf<>"" Then 
					comandos(cc)=sf
					cc+=1
					' NOTA: estos comandos deberian haber sido eliminados anteriormente en "elimina_engorros"
					' pero hago doble comprobacion, por que algunos podrian haberse "colado"
					If sf="static" Then cc-=1:comandos(cc)=""
					If sf="volatile" Then cc-=1:comandos(cc)="" 
					If sf="register" Then cc-=1:comandos(cc)="" 
					If sf="extern" Then cc-=1:comandos(cc)="" 
					If sf="const" Then cc-=1:comandos(cc)="" 
				EndIf		
				sf=""
			End If

			
			' si hemos encontrado un caracter que indique fin de busqueda
			' aqui vienen MUCHISIMOS problemas
			' por ejemplo:
			' ¿estamos dentro un grupo de llaves de valor 1? (nllave=1 o "hayllave=x")
			' entonces, es que estamos dentro de una subrutina, por lo tanto TODO se deberia tratar como variable LOCAL
			' pero si estamos fuera de llave (nllave=0 o hayllave=0), entonces puede ser declaracion de SUB o FUNCION
			' o variable GLOBAL.
			' ademas, si estamos fuera de llave, puede ser variable sin definicion o con definicion tipo {}
			If fininstruccion Then 
				longitud=R-inicio ' longitud total de caracteres leidos

					' muestro datos , depuracion solo
					'micolor(amarillo)
					'Print "instruccion a tratar:"
					'Print Mid(tcpp,inicio,longitud)
					'Print
					'micolor(gris)
			
				' el caso raro typedef de una sola linea, con mi trampa Xype que indica que lo es
				If comandos(0)="Xype" Then 
					cambiatexto(inicio,"T")
					sa=leetexto(R,128)
					a=InStr(sa,";") ' busco el final
					'Print leetexto(inicio,(R-inicio)+a)
					R=R+a ' actualizo puntero al final de la linea , al ";"
					GoTo esXype ' me salto todo
				EndIf		
							
				' si es un TYPEDEF, lo trato por separado (puede ser TYPE o ENUM)
				If comandos(0)="Type" Then 
					fininstruccion=0 ' para que no se ejecuten mas cosas al acabar aqui
					If comandos(1)="Enum" Then sb="Enum":sc="End Enum" Else sb="Type":sc="End Type"
					For gg=inicio To tcar ' busco el inicio de la llave
						leetexto(gg,1)
						If nllave=1 Then inillave=gg:Exit For
					Next	
					For gg=inillave To tcar ' busco el final de la llave
						leetexto(gg,1)
						If nllave=0 Then finllave=gg-1:Exit For
					Next	
					aa=InStr(finllave,tcpp,";") ' y ahora, el ";" que indica fin de comando TYPEDEF
					sa=(Mid(tcpp,finllave+1,aa-finllave-1)) ' pillo el nombre de la variable , que va al final del TYPEDEF
					' y llegan los cambios
					cambiatexto(inillave," ") ' borro la llave inicial
					cambiatexto(finllave," ") ' borro la llave final
					borratexto(finllave+1,Len(sa)) ' borro el nombre de la variable, que va al final, tras la llave
					insertatexto(finllave+1,Len(sc)) ' hacemos sitio para meter el END TYPE o END ENUM
					cambiatexto(finllave+1,sc) ' y metemos el nuevo
					' ahora el inicio		
					sa=sb+" "+trim(sa) ' construyo el "Type fulanito" o "Enum menganito"
					longitud=inillave-inicio ' longitud a tratar, entre la "T" de Typedef y la llave "{"
					borratexto(inicio,longitud) ' borro lo viejo
					insertatexto(inicio,Len(sa)) ' hacemos sitio para lo nuevo
					cambiatexto(inicio,sa) ' y metemos el nuevo
					inillave=inicio+Len(sa) ' nueva posicion de la llave "{" (ya no esta, pero si su posicion)
					finllave=finllave-longitud+Len(sa)+Len(sc) ' nueva posicion de la llave "}" final (ya no esta, pero si su posicion)
					' y por ultimo las variables que van dentro
							'''''''''''''''' este PUTOOOO rollo paso de documentarlo ''''''''''''''''''''
							'''''''''''''''' estuve todo un PUTO dia solo para esta PUTA MIERDA de rutina que arregla lo params. del TYPE
							'''''''''''''''' si lo quieres entender, haya tu. pero casi mejor anularla en caso de fallo y que le den POR CULO
					dd=inillave ' contador para ir avanzando dentro del grupo de "{}"
					If sb="Type" Then ' solo en TYPE, ENUM no lleva variables, sino enumeraciones
						For ff=0 To 10000 'busco hasta 10000 letras (espero que cubra un TYPE grande)
							bb=inillave+ff ' empiezo en el inicio de la llave (que ya no existe, pero si su posicion)
							sa=leetexto(bb,1)
							If nllave=0 Then Exit For ' hemos salido de las llaves, acabamos, en teoria, no hay mas parametros
							If tipo=CRLF Then
								sc=mid(tcpp,dd,bb-dd)
								'Print "a tratar:";sc;"<<<<<<<<"
								sd=""
								If sc<>"" Then 
									For gg=1 To 100
										sa=Mid(sc,gg,1)
										If esletra(sa)>0 Then 
											For aa=gg To 100
												sa=Mid(sc,aa,1)
												If esletra(sa)>0 Then sd=sd+sa Else Exit for
											Next
											'Print Mid(tcpp,dd+(gg-1),Len(sd))
											aa=Len(sd)
											borratexto(dd+(gg-1),Len(sd))
											sd="As "+cambia_variable(sd)
											insertatexto(dd+(gg-1),Len(sd))
											cambiatexto(dd+(gg-1),sd)
											bb=bb+(Len(sd)-aa)-1
											'Print Mid(tcpp,dd+(gg-1),20)
											Exit for
										EndIf
									Next
								EndIf
								dd=bb+1
								'Sleep
							End If
						Next	
						For ff=bb To bb+100
							sa=leetexto(ff,1)
							If tipo=CRLF Then Exit For
						Next
						finllave=ff ' actualizo posicion final
					End If
					''''''
					R=finllave ' actualizamos posicion tras el ";" del final del type				
				EndIf
				
					
'''''''''''''''''''''''''''''''''''
				' lo dificil..... y de cojones, llevo DIAS estudiando TODAS las posibilidades.... BUUUUFFFF!!!!!
				' ver si es declaracion de SUB o FUNCION, subrutina como tal o variable GLOBAL o LOCAL.... casi nada
				
					
				' esto podria ser FUNCION o SUB, con o sin DECLARE , depende de si lleva "{}"
				If fininstruccion=2 then' es sub o funcion, por que hay un "(" tras los nombres
					'''''''''''''''''''
					' buscamos el grupo entre "()" de valor 1
					For ff=R To tcar ' R se supone que es el primer "(" , por que arriba lo hemos detectado
						sa=leetexto(ff,1)
						If sa=")" And nparentesis=1 Then Exit For ' y como el primer "(" siempre es 1, buscamos que sea "0"
					Next
					' lo reparamos (cambiando su orden)
					ff-=1 ' retrocedo una posicion, para que quede antes del ")"
					R+=1 ' salto una posicion, para ponerme delante del "(" (ahora mismo, R apunta al "(")
					params=leetexto(R,ff-R+1) ' los leo del origen entre "()" (OJO: ENTRE "()", SIN INCLUIRLOS)
					borratexto(R,Len(params)) ' borro los parametros viejos
					params=invierte_parametros(params) ' pues eso, los invierte, y los deja al estilo FB (o sea, "int pep" por "pep as integer")
					insertatexto(R,Len(params)) ' hago sitio
					cambiatexto(R,params) ' meto los nuevos
					''''
					R=R+Len(params)+1 ' actualizo el punto de lectura, al final del ")"
					params="("+params+")" ' añado parentesis a los params, para luego, que me hacen falta con ellos incluidos
					'''''''''''''''''''
					' con la R actualizada, busco grupo "{}" para ver si es DECLARACION o SUBRUTINA/FUNCION
					rr=0
					For ff=R To tcar ' R ahora es el ")" localizado aqui atras
						sa=leetexto(ff,1)
						' lo anulo, es imposible este caso --> If sa="=" Then Exit For ' si aparece antes que la llave, es variable
						If sa=";" Then Exit For ' si aparece ANTES que la llave, es DECLARE de sub/funcion
						If nllave=1 Then ' si hay llave, buscamos principio y final
							inillave=ff
							For gg=inillave To tcar
								leetexto(gg,1)
								If nllave=0 Then finllave=gg-1:Exit For ' sa acaban las llaves=1,ya tenemos fin de llave
							Next								
						EndIf
						If inillave Then Exit For ' salimos si ya tenemos datos INILLAVE y FINLLAVE
					Next		
					'''''''''''''''''''
					' si tras buscar "{" , ANTES ha aparecido otra cosa:
					sc=""
					If inillave=0 Then 
						sc="Declare " ' si NO hay llave, es DECLARE
					EndIf
					'''''''''''''''''''
					' tratamos la SUB o FUNCTION (con o sin DECLARE delante)
					If lCase(comandos(0))="void" Or comandos(1)="" Then ' si hay un VOID es SUB, o si SOLO hay un parametro (el 0)
						' el segundo comando es el nombre de la sub
						sa=comandos(1)
						If sa="" Then sa=comandos(0) ' si SOLO hay un parametro (tipico de sun SUB "a pelo" del tipo "pepe(int a)"
						' si aparece un "*" suelto, es posible puntero, pero se ha escrito separado de su variable o nombre
						If sa="*" Then sa="*"+comandos(2) ' hay casos, que el "*" esta separado --> pepe * pipo(ppe); deberia ser pepe *pipo(ppe);						
						' reconstruimos una FUNCION tipo FB
						sa=sc+"Sub "+sa+params  ' esta es la reconsturccion
						sc="End Sub" ' para poner el final luego, pero puede que NO se use, depende.... (ver abajo)
					Else ' si no, puede ser FUNCION
						' suponemos que el primer comando es el tipo de variable, personal o fija tipo C
						sa=cambia_variable(comandos(0)) ' cambiamos tipo C a tipo FB (ejemplo: int a integer)
						' y suponemos que el segundo comando es el nombre de la funcion
						sb=comandos(1)
						' si aparece un "*" suelto, es posible puntero, pero se ha escrito separado de su variable o nombre
						If sb="*" Then sb="*"+comandos(2) ' hay casos, que el "*" esta separado --> pepe * pipo(ppe); deberia ser pepe *pipo(ppe);						
						' reconstruimos una FUNCION tipo FB
						sa=sc+"Function "+sb+params+" As "+sa  ' esta es la reconstruccion
						sc="End Function" ' para poner el final luego, pero puede que NO se use, depende.... (ver abajo)
					End If
					'''''''''''''''''''			
					longitud=longitud+Len(params) ' sumo la long. de los params entre "()"
						
					If Trim(Mid(tcpp,inicio,longitud))="" Then Print "ERROR: no se reconoce comando":Print leetexto(R-10,40):ACABAR(R+20)											
					'Print "entrada : ";Mid(tcpp,inicio,longitud)
					'Print "salida  : ";sa:Sleep
					
					' ya podemos reconstruir todo
					' tenemos "sa" como entrada a cambiar en la pos. "inicio" con longitud "longitud"
					' y final "posible" de llave en "inillave" (solo si es subrutina, no declaracion)
					' antes, miramos si debo meter el final clasico "END SUB/FUNCTION"
					' hay que hacerlo ANTES de meter el principio, por que sino, cambiamos la pos. "inillave"
					If inillave Then ' si es=1, no es bueno, al menos 2, que son {} las dos llaves
						insertatexto(finllave,Len(sc)-1) ' hacemos sitio para lo nuevo (la llave cuenta tambien, por eso resto uno)
						cambiatexto(finllave,sc) ' y metemos el nuevo
						borratexto(inillave,1) ' borro la llave inicial
						R = (finllave + Len(sc) -1)
					End If
					' ahora que ya da igual si tocamos el tamaño del TCPP, metemos el inicio
					borratexto(inicio,longitud) ' borro lo viejo
					insertatexto(inicio,Len(sa)) ' hacemos sitio para lo nuevo
					cambiatexto(inicio,sa) ' y metemos el nuevo
					' calculo la nueva posicion en la que seguir la busqueda, depende de si hay {} o no, debo ponerme al final
					If inillave Then 
						R = (finllave + Len(sc)) - longitud + Len(sa)-1 ' en caso de ser SUB/FUNCTION me pongo tras el grupo {}
					Else
						R = inicio + Len(sa) ' si es DECLARE me pongo tras el ultimo ")" o justo sobre el ";"
					EndIf
					'R=inicio':Print R:sleep
					'Locate 1,1:Print inicio,longitud,sc,hayllave
					'miprint()
					
					
				' el resto, podrian ser variables GLOBAL (esta FUERA de llaves {} valor 1)
				ElseIf fininstruccion=1 Then 

					If hayigual Then longitud=hayigual-inicio ' esto es para dejarlo como ANTES del "=" ("hayigual" solo se usa en DECLARES, no aqui)
					' suponemos que el primer comando es el tipo de variable, personal o fija tipo C (nota: ya habremos filtrado "static" o "struct" arriba)
					If comandos(0)="unsigned" Or comandos(0)="signed" Then 
						If comandos(1)="int" Or comandos(1)="char" or comandos(1)="short" or comandos(1)="long" Then 
							' casos con unsigned delante del tipo
							sa=cambia_variable(comandos(0)+" "+comandos(1)) ' cambiamos tipo C a tipo FB (ejemplo: int a integer)
							sb=comandos(2)	' y suponemos que el tercer comando es el nombre de la variable  
						Else
							' casos con SOLO unsigned, sin indicar tipo, sino directo variable tipo UNSIGNED PEPE=0
							sa=cambia_variable(comandos(0)+" int") ' cambiamos tipo C a tipo FB (ejemplo: int a integer)
							sb=comandos(1)	' y suponemos que el segundo comando es el nombre de la variable  
						End If
					Else
						' resto de casos (sin unsigned delante)
						sa=cambia_variable(comandos(0)) ' cambiamos tipo C a tipo FB (ejemplo: int a integer)
						sb=comandos(1) ' y suponemos que el segundo comando es el nombre de la variable  
					EndIf
					' si aparece un "*" suelto, es posible puntero, pero se ha escrito separado de su variable o nombre
					If sb="*" Then sb="*"+comandos(2) ' hay casos, que el "*" esta separado --> pepe * pipo(ppe); deberia ser pepe *pipo(ppe);						
					' reconstruimos una variable GLOBAL tipo FB
					sa="Dim Shared As "+sa+" "+sb
					borratexto(inicio,longitud) ' borro lo viejo
					insertatexto(inicio,Len(sa)) ' hacemos sitio para lo nuevo
					cambiatexto(inicio,sa) ' y metemos el nuevo
					For ff=inicio+Len(sa) To tcar
						leetexto(ff,1)
						If tipo=PUNTOYCOMA Then R=ff+1:Exit for
					Next
					'miprint()
					
					
				' y aqui acabamos (acabamos?) va aser que no, siguen saliendo cosas (junio-2020) ..... no se que mas hacer!!!!
				End If
''''''''''''''''''''''''''''''''''''''''''''		
	
				' si es un TYPEDEF de una sola linea, tipo "typedef int pepe;" llega aqui (un trampa que pone Xype en vez de Type para distinguirlos)	  	
			  	esXype:
			  	
			  	'Print
			  	'For ff=0 To cc-1
			  	'	Print comandos(ff)
			  	'Next
			  	'Print

			  	
			  	'Sleep

				' a cero todo, para otra busqueda despues
				For ff=0 To cc-1:	comandos(ff)="":Next:cc=0 ' contador de comandos a cero
				inicio=0
				longitud=0
				fininstruccion=0
				hayigual=0
				inillave=0
				finllave=0
			end If	
			
			
		End If ' fin "IF tipo<DEFINES"

  	Wend


End Sub


' ------------------------------------------------------------------------------------------------------------
' el TYPEDEF es un ALIAS, que usado junto a STRUCT evita que se deba identificar cada variable como STRUCT
' es darle un nombre REAL a una ESTRUCTURA para no tener que escribir STRUCT todo el rato
' en una definicion como esta:
'     struct S {int a; }
' como NO usamos TYPEDEF, estamos obligados a poner STRUCT SIEMPRE:
' void subrutina(struct S pepe) (nota la presencia de "struct" delante de la variable S)
' pero si se define con TYPEDEF delante, y no hace falta el STRUCT al crear variables:
'     typedef struct S {int a; }
' y ahora la subrutina de antes seria:
' void subrutina(S pepe) (nota la falta de "struct" delante de la variable S) 
' osea TYPEDEF crea un alias para abreviar el empleo de variables o estructuras
' ejemplo:
'     typedef int variableentera;
'     variableentera pepe; // definimos pepe como 'variableentera', o sea, como 'int' tal cual se ve en el typedef
' el problema del typedef es la variedad de formas:
' 		typedef struct S {int a} S;
' 		typedef struct {int a} S;
' 		typedef {int a} S;
' son las tres IGUALES, todas conducen a lo mismo
' pero esta es distinta!!!!
'		typedef struct S {int a} T; // nota la diferencia entre struct S y typedef T (alias T)
' o esta forma:
' 		struct S {int a};
'		typedef struct S T;
' buscar todo es un quebradero de cabeza. pero en general:
'   - si esta solo el struct, cambiar por TYPE sin mas.
'   - si lleva typedef y 2 variables, delante-detras, si es la misma , coger una y cambiar typedef struct pot TYPE
'                       si son distintas, tratar de separar con 2 type, uno para la estructura, otro para el alias
'   - si va SIN llaves {}, mirar si lleva struct y quitarlo, y luego reconstruir como si fuera un DIM, pero con TYPE
' 
' variedades encontradas:
'   	typedef int T; // alias T para INT
'   	typedef int T[1]; // idem, pero array, es problema porque basic no lo acepta
'
'     struct S { int T }; // solo estructura, sin alias, pero.....
'   	typedef struct S R; // .... como ya existe estructura S, le damos el alias R
'
'   	typedef { int T } S; // sin 'struct', alias S, caso mas empleado en BASIC
'   	typedef struct { int T } S; // alias y estructura se llaman igual, o sea S
'   	typedef struct S { int T } S; // idem caso anterior, pero se especifica
'   	typedef struct S { int T } R; // alias R a la estructura S

Sub trata_typedef_struct_enum()
	
	Dim As Integer aa,bb,cc,dd,ee,ff, posvar1,posvar2
	Dim As String  sa,sb,sc,sd
	Dim As String  todo,parte,var1,var2 ' toda la linea, solo entre {}, nombre variable inicial, nombre varaible final
	Dim As Integer typedef=0
	
	' RGB
	Dim R As Integer ' principal, por todo el fichero
	Dim G As Integer ' secundario, para cada grupo encontrado en el IF
	Dim B As Integer ' auxilar, para dentro de cada mini-grupo, dentro del grupo G


	R=0
	While 1
	   sc="typedef enum" ' posibilidad mas comun de ENUM, seguido de TYPEDEF
		sb=leetexto(R,Len(sc))
		If sb=sc And tipo<DEFINES Then
			sb=leetexto(R-1,Len(sc)+2)
			If (esletra(Left(sb,1))=0) and (esletra(right(sb,1))=0) Then  ' anterior y posterior NO son letra
				'If Right(sb,1)=" " Then sb=""' si hay un espacio seguido, lo quito tambien
				'If Right(sb,1)=Chr(9) Then sb=""' si hay un TAB seguido, lo quito tambien
				'insertatexto(aa,1)
				borratexto(R,3) ' quito 3 letras
				cambiatexto(R,"Type Enum")
				typedef=1 ' para buscar el nombre seguido de salir de aqui
				R+=8 ' me pongo detras
				' me salto el espacio o tabulacion inicial, si hay mas, a joderse y rezar
				If leetexto(R,1)=" " Or leetexto(R,1)=Chr(9) Then R+=1 
			End If
		EndIf
		
	   sc="typedef" 
		sb=leetexto(R,Len(sc))
		If sb=sc And tipo<DEFINES Then
			' antes veo si hay enum detras, por que puede venir separado por tabulacion o mas de un espacio
			sd="enum"
			For ff=0 To 32
				sb=leetexto(R+ff,Len(sd))
				If sb=sd And tipo<DEFINES Then
					sb=leetexto(R-1+ff,Len(sd)+2)
					If (esletra(Left(sb,1))=0) and (esletra(right(sb,1))=0) Then  ' anterior y posterior NO son letra
						borratexto(R,3+(ff-8)) ' quito las letras que sobran, que incluyen espacios y tabs
						cambiatexto(R,"Type Enum")
						typedef=1 ' para buscar el nombre seguido de salir de aqui
						R+=8 ' me pongo detras
						GoTo typestruct ' salgo
					End If
				EndIf
			Next
			' si no hay un enum seguido, lo tratamos como TYPE
			sb=leetexto(R-1,Len(sc)+2)
			If (esletra(Left(sb,1))=0) and (esletra(right(sb,1))=0) Then  ' anterior y posterior NO son letra
				'If Right(sb,1)=" " Then sb=""' si hay un espacio seguido, lo quito tambien
				'If Right(sb,1)=Chr(9) Then sb=""' si hay un TAB seguido, lo quito tambien
				'insertatexto(aa,1)
				borratexto(R,3) ' quito tres letras (del typedef me quedo con type solo)
				cambiatexto(R,"Type")
				typedef=1 ' para buscar el nombre seguido de salir de aqui
				R+=4 ' me pongo detras
				' me salto el espacio o tabulacion inicial, si hay mas, a joderse y rezar
				If leetexto(R,1)=" " Or leetexto(R,1)=Chr(9) Then R+=1 
			End If
		EndIf
		
	   sc="struct"
		sb=leetexto(R,Len(sc))
		If sb=sc And tipo<DEFINES Then
			sb=leetexto(R-1,Len(sc)+2)
			If (esletra(Left(sb,1))=0) and (esletra(right(sb,1))=0) Then  ' anterior y posterior NO son letra
				'If Right(sb,1)=" " Then sb=""' si hay un espacio seguido, lo quito tambien
				'If Right(sb,1)=Chr(9) Then sb=""' si hay un TAB seguido, lo quito tambien
				If typedef=1 Then ' si antes ya ha pillado un typedef, este struct se elimina
					' esto es asi para casos juntitos, como "typedef struct pepe{}"
					borratexto(R,Len(sc)) ' borro todo el struct
					GoTo typestruct ' y salgo del todo
				End If					
				'insertatexto(aa,1)
				borratexto(R,2) ' quito dos letras
				cambiatexto(R,"Type")
				typedef=1 ' para buscar el nombre seguido de salir de aqui
				R+=4 ' me pongo tras el Type recien metido
			End If
		EndIf
		typestruct: ' para salir aqui si aparecen juntos typedef y struct


	'Wend

	
	' primero los TYPEDEF
	' aquie hace muchas cosas, pero algunas las he tapado y solo he dejado el tratamiento de los tpedef en una sola linea
	' dado que la rutina instrucciones() hace los casos mas complejos.
	' aqui hay mucha morralla, pero solo sirve la parte que trata una linea (ver donde pone "X", abajo casi)
	'sc="typedef" '"struct"
	'R=0
	'typedef=0
	'While 1
	  	'If (R>tcar) Then exit while ' hasta que no alcance el total de caracteres
	  	'R+=1

		'sb=leetexto(R,Len(sc))
		'If sb=sc And tipo<DEFINES Then
		'	sb=leetexto(R-1,Len(sc)+2)
		'	If (esletra(Left(sb,1))=0) and (esletra(right(sb,1))=0) Then  ' anterior y posterior NO son letra
		'		'If Right(sb,1)=" " Then sb=" " Else sb=""' si hay un espacio seguido, lo quito tambien
		'		'insertatexto(aa,1)
		'		borratexto(R,3) ' quito tres letras (del typedef me quedo con type solo)
		'		cambiatexto(R,"Type")
		'		typedef=1 ' para buscar el nombre seguido de salir de aqui
		'		R+=4 ' me pongo tras el Type recien metido
		'	End If
		'EndIf

		' segundo, organizar los typedef
		' si se ha localizado un typedef, actuamos, buscando mas cosas, como el nombre que lleva al final.
		If typedef>0 Then
			'sa=leetexto(R,15) ' leo 7c. para pillar un posible 'struct' dentro de ellos
			''sa=Trim(sa) ' fuera espacios
			'aa=InStr(sa,"struct")
			'If aa Then
			'	borratexto(R+(aa-1),6) ' quito tres letras (del typedef me quedo con type solo)
			'EndIf
			' buscamos el ; final, y verifico si de por medio hay llaves {}
			
			aa=0
			bb=0
			dd=0
			cc=0
			ee=1000
			posvar1=0
			posvar2=0
			
			For ff=0 To tcar ' hasta el infinito.... y mas alla....
				sb=leetexto(R+ff,1)
				If tipo<DEFINES Then
					If sb="{" And bb=0 Then 
						bb=ff:ee=nllave
					EndIf
					If sb="}" And nllave=ee And cc=0 Then cc=ff
					If sb=";" And tipo=PUNTOYCOMA Then 
						If bb=0 Then dd=ff:Exit For
						If cc<>0 Then dd=ff:Exit for
					EndIf
				End If
			Next ff	
			
			
			' al salir tenemos que dd indica la linea completa, desde el ultimo struct o typedef hasta el final (si lleva {} tambien)
			todo=""
			var1=""
			var2=""
			parte=""
			If dd Then
				todo=leetexto(R,dd)
				' cojo la posible variable inicial
				aa=0
				For ff=1 To Len(todo)
					sb=Mid(todo,ff,1)
					If sb="{" And aa=0 Then Exit For
					If esletra(sb) And aa=0 Then aa=ff
					If esletra(sb)=0 And aa>0 Then 
						var1=Mid(todo,aa,ff-aa)
						posvar1=aa-1
						Exit For
					EndIf
				Next ff
				' cojo la posible variable final
				aa=0
				For ff=Len(todo) To 1 Step -1
					sb=Mid(todo,ff,1)
					If sb="}" And aa=0 Then posvar2=ff:Exit For
					If esletra(sb) And aa=0 Then aa=ff
					If esletra(sb)=0 And aa>0 Then 
						var2=Mid(todo,ff+1,aa-ff)
						posvar2=ff+1
						Exit For
					EndIf
				Next ff
			End If

			' y aqui lo que va entre llaves {} si es que existen
			If bb>0 And cc>0 Then
				parte=leetexto(R+bb+1,cc-bb-1)
			End If
			
			' si la linea "parece" una sola, sin {}, puede ser tipo "typedef int pepe;"
			If InStr(todo,"{")=0 And InStr(todo,"}")=0 Then
				insertatexto(R,3)
				cambiatexto(R,"As ")
				cambiatexto(R-5,"X") ' lo trampeo como "Xype" en vez de "Type", y asi, la rutina "instrucciones()" lo arregla
				GoTo lineatypesola
			End If
			
			'Cls
			'Print todo ' linea entera
			'Print ">>>>";var1;"<<<< >>>>";var2;"<<<<" ' variables del. y tras.
			'Print "-------"
			'Print parte ' solo contenido entre {}
			'Sleep
			'Print posvar1,posvar2
			'Print leetexto(R+posvar1,Len(var1));Len(var1),leetexto(R+posvar2,Len(var2));Len(var2)
			'print

			' si var2 es cero (la de abajo), uso la var1, sino, me invento una para que no de error
			If var2="" And var1<>"" Then 
				insertatexto(R+posvar2,Len(var1))
				cambiatexto(R+posvar2,var1) ' meto la VAR1 abajo
				' ponemos a "0" el tipo de variable insertada, sino, se confunde, y le asiina como "llave"
				For ff=0 To Len(var1)-1:Mid(tdat,R+ff+posvar2,1)=Chr(0):Mid(tlla,R+ff+posvar2,1)=Chr(0):Next
				borratexto(R+posvar1,Len(var1))  ' y la borro de arriba
			ElseIf var1="" And var2="" Then ' ambas vacias, pongo algo para evitarel error de mi rutina
				var1="VACIA"
				insertatexto(R+posvar2,Len(var1))
				cambiatexto(R+posvar2,var1)		
				' ponemos a "0" el tipo de variable insertada, sino, se confunde, y le asiina como "llave"
				For ff=0 To Len(var1)-1:Mid(tdat,R+ff+posvar2,1)=Chr(0):Mid(tlla,R+ff+posvar2,1)=Chr(0):Next		
			EndIf

			lineatypesola:
			
			typedef=0
		EndIf
		
			
		  	
		If (R>tcar) Then exit while ' hasta que no alcance el total de caracteres
	  	R+=1
	Wend
'miprint(0)
	'ACABAR(R)


	' por ultimo, los STRUCT, si es que quedan tras pasar por los dos anteriores pasos
	'sc="struct" '"struct"
	'R=0
	'While 1
	'  	If (R>tcar) Then exit while ' hasta que no alcance el total de caracteres
	'  	R+=1

	'	sb=leetexto(R,Len(sc))
	'	If sb=sc And tipo<DEFINES Then
	'		sb=leetexto(R-1,Len(sc)+2)
	'		If (esletra(Left(sb,1))=0) and (esletra(right(sb,1))=0) Then  ' anterior y posterior NO son letra
	'			'If Right(sb,1)=" " Then sb=" " Else sb=""' si hay un espacio seguido, lo quito tambien
	'			insertatexto(R,1)
	'			'borratexto(R,2) ' quito tres letras (del typedef me quedo con type solo)
	'			cambiatexto(R,"typedef") ' lo dejo como typedef, para que la rutina instrucciones() lo arregle
	'			'Print leetexto(R,32):sleep
	'		End If
	'	EndIf
	'	
	'Wend


End Sub
