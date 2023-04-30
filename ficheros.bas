




' Rutina FINAL del todo, la que acaba, cierra, guarda y demas
Sub ACABAR(gg As integer)
	Dim As Integer ff,bb,dd

	' trabajo acabado, sacamos el nuevo fichero
	Open fich_salida For Output As 2
	sc=""
	bb=0
	dd=0
	For ff=1 To gg
		sb=leetexto(ff,1,0) 'Mid(tcpp,F,1)
		
		' si vamos a quitar los comentarios (ver la variable en "variables")
		If tipo>=REM1 And quitacomentarios=1 Then 
			dd=1
			sb=""
		EndIf
		
		If sb=Chr(0) Then sb=""
		If sb=Chr(CRLF) Then sb="":bb=1

		sc=sc+sb
		If bb Then 
			If dd=1 Then ' si dd=1 es que hemos decidido quitar comentarios
				If Trim(sc)<>"" Then
					Print #2,sc
					bb=0:sc="":sb="":dd=0
				Else
					bb=0:sc="":sb="":dd=0
				End If
			Else
				Print #2,sc
				bb=0:sc="":sb=""	
			EndIf
		EndIf
	Next
	
		
		' repito lo mismo al acabar, por si acaso ha quedado algo sin guardar en SC
		If bb Then 
			If dd=1 Then 
				If Trim(sc)<>"" Then
					Print #2,sc
					bb=0:sc="":sb="":dd=0
				Else
					bb=0:sc="":sb="":dd=0
				End If
			Else
				Print #2,sc
				bb=0:sc="":sb=""	
			EndIf
		EndIf
	
	
	Close 2
	Print "ACABADO....":Sleep
	
	'''''
	 End
	'''''
End Sub









Sub leer_fichero()
	Dim As Integer ff

	''''''''''''''''''''''''''''''''''''''''
	' leemos el 'C' entero para un primer uso
	Print "Leyendo fichero:";fich_entrada
	Open fich_entrada For Input As 1
	
		tcpp=""
	
		' tragamos todo el fichero
	  	While Not Eof(1)
	  		Line input #1,sa
	  		sa=RTrim(sa) ' quito los espacios a la derecha solo, util sobre todo en los #define que lleva "\" al final
	  		For ff=1 To Len(sa)
	  			sb=Mid(sa,ff,1)
	  			tcpp=tcpp+sb ' voy metiendo caracter a caracter en una matriz temporal
	  		Next
	  		tcpp=tcpp+Chr(CRLF) 'Chr(&h0D) ' añado un indicador de CR(0D) final por ahora
	  	Wend
		tcar=Len(tcpp) ' longitud total a tratar
		tdat=string(tcar,Chr(0)) ' reservo espacio para comentarios, parentesis y comillas
		tlla=string(tcar,Chr(0)) ' espacio exclusivo para llaves
		tpar=string(tcar,Chr(0)) ' espacio exclusivo para parentesis
		
		' ahora, meto los CRLF calculados antes, dentro de TDAT
		For ff=1 To tcar
			If Asc(Mid(tcpp,ff,1))=CRLF Then Mid(tdat,ff,1)=Chr(CRLF)
		Next

	Close 1
	'''''''''''''''''''''''''''''''''''''''

End Sub


