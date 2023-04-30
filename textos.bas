
' pone una cadena de error con la linea donde lo da, y se sale
Sub pon_error(texto As String,zona As Integer)
	Dim As Integer ff,gg
	Dim As String ss
	
	gg=0
	For ff=1 To Len(tcpp)
		If Asc(Mid(tcpp,ff,1))=CRLF Then gg+=1
		If ff>zona Then Exit For
	Next
	Print "Linea:";gg
	
	Color 14,0
	Print texto
	For ff=zona-120 To zona+200 ' 80 caracteres, 20 por detras, 60 por delante
		ss=leetexto(ff,1)'Mid(tcpp,ff,1)
		Print ss;
	Next
	Print
	Color 7,0
	ACABAR(zona+80) ' 80 mas , por si acaso
	
End Sub


' lee una cadena y activa el indicador de tipo
Function leetexto(zona As Integer,lon As Integer,sincometarios As Integer=1 ) As String

	' "sincometarios" es opcional, si se pone a 0 lee todo, si se pone a 1, solo lee FUERA de comentarios y demas
	tipo=Asc(Mid(tdat,zona,1)) ' coge el tipo de caracter que es
	nparentesis=Asc(Mid(tpar,zona,1)) ' Si es cero, pues es que no hay parentesis
	nllave=Asc(Mid(tlla,zona,1)) ' coge la llave en la que estamos (si es "0", estamos fuera de llaves, sino, entre 1 y 99)
	If tipo>DEFINES And sincometarios Then Return " " ' no hace nada si es REM1, REM2 o COMILLAS
	'If tipo>=DEFINES  Then Return "." ' no hace nada si es DEFINE, REM1, REM2 o COMILLAS
	Return Mid(tcpp,zona,lon)
	
End Function

' cambia una cadena indicada en las posiciones (sustituye)
Sub cambiatexto(zona As Integer,cad As String) ' cambia una cadena
	
	Mid(tcpp,zona,Len(cad))=cad ' ojo, que no verifica lo que "pisa", si hay retornos de carro, nos los comemos......
	
	' ACTUALIZO: por ahora, no hago esto, es mejor dejar el dato como esta, para futuras consultas
	' si el texto cambiado esta "por debajo" del tipo (valor) DEFINES (252) se pone a cero
	' por que los textos cambiados dentro de comillas, defines y comentarios DEBEN conservar su tipo
	'If Asc(Mid(tdat,zona,Len(cad)))<DEFINES Then
	'	Mid(tdat,zona,Len(cad))=string(Len(cad),chr(0)) ' a cero todo lo que esta por debajo de 252
	'End If
	
End Sub

' cambio especial para unos pocos comandos. lo que hace es añadir los huecos que se piden y verificar
' si hay espacios delante o detras de donde insertamos, de modo, que si NO hay espacios, los añade
' ejemplo, en el caso (a&=1) quedaria (a and =1) , sin los espacios saldria (aand=1). si ya existen, NO los añade
Sub cambiatexto_espacios(zona As Integer,esp As Integer, cad As String) ' cambia una cadena, añadiendo huecos automaticos
	Dim As Integer a=0
	Dim As Integer b=0
	Dim As Integer c=Len(cad) ' longitud a insertar (menos los ya existentes)
		
	' primero mira si hay espacios delante o detras de lo que vamos a cambiar
	If leetexto(zona-1,1)<>" " Then a=1 ' delante no hay (1c. antes)
	If leetexto(zona+esp,1)<>" " Then b=1 ' detras no hay (sumando "esp" a "zona")
	' añado los espacios necesarios a "c" segun se necesiten delante o detras o ambos
	' y actualizo los espacios a insertar
	If a=1 Then cad=" "+cad: c+=1
	If b=1 Then cad=cad+" ": c+=1
	
	' añade los huecos solicitados lo primero, para acomodar la diferencia de lo cambiado por lo nuevo
	insertatexto(zona,c-esp) ' rollo: tengo que restar a la long. de "cad", los "esp" y sumar los A y B.... brrrr

	' y hacemos el cambio
	cambiatexto(zona,cad)
	
End Sub

' mete espacios en la cadena (añade longitud)
' para entenderlo: meter un espacio en la pos. del  "=" de "if (pepe=1) pepa()"  , queda como "if (pepe =1) pepa()"
Sub insertatexto(zona As Integer,lon As Integer)
	Dim As String sj
	
	tcpp=Left(tcpp,zona-1)+Space(lon)+Mid(tcpp,zona) ' inserto espacios en la zona de trabajo
	
	sj=Mid(tdat,zona-1,1) ' cogemos dato de la pos anterior que insertamos. es importante, no se coge de ZONA sino de ZONA-1
	If sj=Chr(PUNTOYCOMA) Then sj=Chr(0) ' no permito que se repita PUNTOYCOMA
	tdat=Left(tdat,zona-1)+String(lon,sj)+Mid(tdat,zona) ' copio estado de datos (el que esta sobre la zona a insertar)
	
	sj=Mid(tlla,zona-1,1) ' cogemos llave
	tlla=Left(tlla,zona-1)+String(lon,sj)+Mid(tlla,zona) ' copio estado de llaves (la que esta sobre la zona a insertar)

	sj=Mid(tpar,zona-1,1) ' cogemos parentesis
	tpar=Left(tpar,zona-1)+String(lon,sj)+Mid(tpar,zona) ' copio estado de parentesis
		
	tcar+=lon

	' NOTA: es importante saber que si metemos algo sobre llaves o parentesis, puede que se copie el estado que NO es
	' por ejemplo, si pongo "x" antes de "{" (o sea: "x{") se copia el estado anterior a primera llave , o sea "0", y queda bien
	' pero al "verres", si la "x" es tras "}" (o sea "}x") ya no queda bien, por que ya hemos salido de la llave, pero se copia "1"
	' o sea, se coge el estado anterior, que en este caso es "1", como si seguiriamos en la llave, pero no es cierto.....!!!!
End Sub

' quita caracteres en la posicion indicada (acorta longitud)
Sub borratexto(zona As Integer,lon As Integer)

	tcpp=Left(tcpp,zona-1)+Mid(tcpp,zona+lon)	' zona de textos
	tdat=Left(tdat,zona-1)+Mid(tdat,zona+lon) ' zona de datos
	tlla=Left(tlla,zona-1)+Mid(tlla,zona+lon) ' zona de llaves
	tpar=Left(tpar,zona-1)+Mid(tpar,zona+lon) ' zona de parentesis
	
	tcar-=lon
	
End Sub




' comprueba si es una letra o numero
' 0= ni letra ni numero
' 1= minuscula
' 2= mayuscula
' 3= numero
Function esletra(car As String) As Integer
	If (car>="a" And car<="z") Then Return 1
	If (car>="A" And car<="Z") Then Return 2
	If (car>="0" And car<="9") Or (car=".") Then Return 3 ' numeros, que incluye el punto, para decimales
	If car="_" Or car="&" Or car="*" Then Return 4 
	Return 0
End Function



' IMPORTANTE: para usar esta rutina ACTIVAR el MODO GRAFICO 18 del FB (ver al incio del modulo principal)
Sub miprint(zona As Integer)
	Dim As Integer px,py
	Dim As String pa,pb,pc
	'Static As Integer posic=1,copiaposic ' para avance cada vez que entramos
	Dim As Integer posic,copiaposic 
	Dim As Integer altopan=90 ' filas de altura que podemos mostrar
	Dim As Integer verllaves=0, verparentesis=0

	Cls
	
	posic=zona ' 1 si queremos empezar desde arriba
	
otravez:

	copiaposic=posic ' copia, para rescatar la posicion
	
	' borramos primero
	For px=5 To altopan
		For py=1 To 125
			Locate px,py
			Print " ";
			'micolor(marron) ' azul MUY oscuro para el CLS
			'Print Chr(176); ' los espacios, con el moteado, o sino, con el "."
		Next
	Next
		
	px=5
	py=1

	While 1
		pa=leetexto(posic,1)
		Locate px,py
		
		' la tabulacion "chr(9)" la cuento como "4 espacios" y la pongo de otro color para verla mejor
		If pa=Chr(9) Then 
			micolor(gris4)
			Print string(4,Chr(179));:py+=3 ' TABulacion, cuento 4 espacios y pongo como "|"
		EndIf
		
		' el espacio lo cambio por un "." (chr(249)) que permite apreciar mejor los "huecos"
		if pa=" " Then pa=Chr(249):micolor(gris3):GoTo directo
		
		micolor(blanco) ' blanco por defecto
		If Asc(pa)=CRLF Then micolor(azul2):Print Chr(219):px+=1:py=0:Locate px,py+1 ' el CRLF lo pongo como un bloque 
		If py>125 Then py=125
		If px>altopan Then Exit While
		If tipo=251 Then micolor(cian) ' punto y coma
		If tipo=252 Then micolor(marron) ' defines
		If tipo=253 Then micolor(amarillo) ' comillas
		If tipo=254 Then micolor(cielo) ' rem 1 //
		If tipo=255 Then micolor(morado) ' rem 2 /* */
		
		' colores ROJOS (nota: no toco los comandos mios, los CRLF en adelante)
		If verllaves Then
			If nllave=1 And tipo<CRLF Then Color RGB(250,0,0)
			If nllave=2 And tipo<CRLF Then Color RGB(220,0,0)
			If nllave=3 And tipo<CRLF Then Color RGB(190,0,0)
			If nllave=4 And tipo<CRLF Then Color RGB(160,0,0)
			If nllave=5 And tipo<CRLF Then Color RGB(130,0,0)
			If nllave=6 And tipo<CRLF Then Color RGB(100,0,0)
			If nllave=7 And tipo<CRLF Then Color RGB( 70,0,0)
			If nllave=8 And tipo<CRLF Then Color RGB( 40,0,0)
			If nllave=9 And tipo<CRLF Then Color RGB( 10,0,0)
		EndIf
		
		' colores VERDES
		If verparentesis Then
			If nparentesis=1 Then Color RGB(0,250,0)
			If nparentesis=2 Then Color RGB(0,220,0)
			If nparentesis=3 Then Color RGB(0,190,0)
			If nparentesis=4 Then Color RGB(0,160,0)
			If nparentesis=5 Then Color RGB(0,130,0)
			If nparentesis=6 Then Color RGB(0,100,0)
			If nparentesis=7 Then Color RGB(0, 70,0)
			If nparentesis=8 Then Color RGB(0, 40,0)
			If nparentesis=9 Then Color RGB(0, 10,0)
		EndIf
		
	directo:
		Print pa
		posic+=1
		py+=1
		If posic>tcar Then Exit While
	Wend

	micolor(rosa)
	Locate 1,1
	Print "Visualizando posiciones ";copiaposic;" a ";posic;" de ";tcar
	
	' deteccion de teclas
	Locate 1,1
	pa=""
	While pa="":pa=InKey():Wend
	If len(pa)=2 Then pa=Mid(pa,2,1) ' teclas extendidas
	pa=LCase(pa)
	
	
	' 1: ver grupos de LLAVES {}
	If pa="1" Then verllaves=IIf(verllaves=0,1,0):posic=copiaposic:GoTo otravez  ' vemos con llaves
	
	' 2: ver grupos de PARENTESIS ()
	If pa="2" Then verparentesis=IIf(verparentesis=0,1,0):posic=copiaposic:GoTo otravez ' vemos con parentesis

	
	
	' CURSORES ARRIBA y ABAJO para mover una linea
	if pa="h" Then ' subir linea
		posic=copiaposic
		If posic<2 Then posic=1:GoTo otravez
		For px=posic-1 To 1 Step -1
			sa=leetexto(px,1)
			If sa=Chr(CRLF) And px<posic-2 Then posic=px+1:GoTo otravez
		Next
		posic=1:GoTo otravez
	EndIf
	
	if pa="p" Then  ' bajar linea	
		posic=copiaposic
		posic=InStr(posic+1,tcpp,Chr(CRLF))+1
		If posic=1 Then posic=tcar
		GoTo otravez
	EndIf
	
	
	
	' AVPAG y REPAG para subir/bajar pagina
	if pa="i" Then ' subir pagina
		posic=copiaposic
		posic-=100
		If posic<1 Then posic=1:GoTo otravez
		For px=posic-1 To 1 Step -1
			sa=leetexto(px,1)
			If sa=Chr(CRLF) Then posic=px+1:GoTo otravez
		Next
		posic=1:GoTo otravez
	EndIf
	
	if pa="q" Then ' bajar pagina
		posic=copiaposic
		posic+=100
		posic=InStr(posic+1,tcpp,Chr(CRLF)) ' bajar linea	
		If posic>tcar Then posic=copiaposic
		If posic=0 Then posic=tcar
		GoTo otravez
	EndIf
	
	
	' ESC: salir
	If pa=Chr(27) Then micolor(gris):Cls:Exit sub
	
	
	' cualquier otra tecla, no hace nada
	posic=copiaposic
	GoTo otravez
	
	
End Sub
