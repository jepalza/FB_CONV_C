' nombre del fichero de trabajo
Dim Shared As String fich_entrada
Dim Shared As String fich_salida
Dim Shared As String fich_ruta


' variables
Dim Shared As String sa,sb,sc,sd
Dim Shared As Integer P ' puntero usado en varias rutinas
Dim Shared As Integer a ' usada solo en el principal como ayuda

Dim Shared As String datosvariable
Dim Shared As integer cambiar ' contador de lineas en las que se cambia algo


' aqui va el fichero completo a tratar y sus datos extraidos por mi
Dim Shared tcpp As String'*(100*1024) ' fichero completo en una sola linea, por caracteres uno a uno
Dim Shared tdat As String'*(100*1024) ' zona donde almaceno la posicion de los comentarios, comillas, defines, punto y coma y CRLF
Dim Shared tlla As String'*(100*1024) ' zona donde almaceno la posicion de las llaves exclusivamente
Dim Shared tpar As String'*(100*1024) ' zona donde almaceno la posicion de los parentesis exclusivamente
Dim Shared tcar As Integer ' total de caracteres leidos, o sea, la longitud de la cadena
Dim Shared tipo As Integer ' almacena el tipo de caracter, bien sea llave, parentesis, comentario, CR, etc

' valores de llaves y parentesis en los que nos encontramos
Dim Shared As Integer nllave=0 ' por defecto, 0 es "fuera de"
Dim Shared As Integer nparentesis=0 ' por defecto, 0 es "fuera de"

' almacen para textos encontrados antes de un parentesis, llave o "="
Dim Shared comandos(30) As String





' funciones y rutinas
Declare Function leetexto(zona As Integer, lon As Integer,sincometarios As Integer=1) As String ' lee una cadena
Declare function esletra(car As String) As Integer
Declare Function cambia_variable(ByVal cad As String) As String ' cambia variable tipo C a basic (float a single, etc)
Declare Function invierte_parametros(ByVal params As String) As String ' da la vuelta a los parametros

Dim Shared As Integer quitacomentarios=0 ' si quieremos quitar los comentarios al guardar el resultado, poner aqui "1"
Declare function comentarios(pp As Integer) As Integer ' aisla comentarios

Declare Sub miprint(zona As Integer) ' para ver el fichero en pantalla, mientras depuro
Declare Sub pon_error(texto As String,zona As Integer)
Declare Sub micolor(a As uInteger)

Declare Sub cambiatexto_espacios(zona As Integer, esp As Integer, cad As String)  ' cambia una cadena de un modo especial, añadiendo espacios (ver declaracion)

Declare Sub cambiatexto   (zona As Integer, cad As String)  ' cambia una cadena
Declare sub insertatexto  (zona As Integer, lon As Integer) ' añade texto
Declare Sub borratexto    (zona As integer, lon As Integer) ' los quita
Declare Sub copiavariable (zona As Integer) ' copia la variable anterior a la zona, para reconstruir cosas tipo "a++"
Declare Sub trata_iif     (zona As integer) ' cambia el formato (a?1:0) por IIF(a,1,0)

Declare Sub trata_if()
Declare Sub trata_case()
Declare Sub trata_for()
Declare Sub trata_while()

Declare Sub leer_fichero()
Declare Sub ACABAR(gg As integer)

Declare Sub operadores()
Declare Sub cambios_basicos()
Declare Sub cambia_puntoycoma()
Declare Sub quita_dospuntos()
Declare Sub busca_variables() 

Declare Sub aisla_grupos() ' para encontrar los comentarios, comillas y "#"
Declare Sub busca_comentarios(zona As Integer) ' aqui se llama desde "aisla_grupos" cuando cree haber localizado uno

Declare Sub busca_llaves_parentesis_puntoycoma() ' para encontrar los comentarios, {} y () y darles un identificador de trabajo




Enum
	CRLF=250 ' sustituye CR+LF por solo CRLF (de 2 a 1 byte)
	PUNTOYCOMA=251
	DEFINES=252 ' elementos con "#" delante, como INCLUDE, IFDEF, DEFINE, IFNDEF, ENDIF, etc.
	COMILLAS=253
	REM1=254 ' tipo "corto" del estilo //
	REM2=255 ' tipo "largo" del estilo /* */
End Enum




''''''''''''''''''''
'Dim Shared As Byte mantenercomentarios=1 ' si lo ponemos a "0" no se guarda NINGUNO, por defecto, se guardan
''''''''''''''''''''


' colores para depuracion solo
Dim Shared As Integer blanco=RGB(255,255,255)
Dim Shared As Integer negro =RGB(0,0,0)
Dim Shared As Integer cielo=RGB(0,150,255)
Dim Shared As Integer marron=RGB(150,50,0)
Dim Shared As Integer rosa=RGB(249,125,182)

Dim Shared As Integer rojo=RGB(255,0,0)
Dim Shared As Integer verde=RGB(0,255,0)
Dim Shared As Integer azul=RGB(0,0,255)
'
Dim Shared As Integer rojo2=RGB(190,0,0)
Dim Shared As Integer verde2=RGB(0,190,0)
Dim Shared As Integer azul2=RGB(0,0,190)
'
Dim Shared As Integer rojo3=RGB(120,0,0)
Dim Shared As Integer verde3=RGB(0,120,0)
Dim Shared As Integer azul3=RGB(0,0,120)
'
Dim Shared As Integer amarillo=RGB(255,255,0)
Dim Shared As Integer amarillo2=RGB(200,200,0)
Dim Shared As Integer amarillo3=RGB(120,120,0)
'
Dim Shared As Integer cian=RGB(0,255,255)
Dim Shared As Integer cian2=RGB(0,170,170)
Dim Shared As Integer cian3=RGB(0,85,85)
'
Dim Shared As Integer morado=RGB(255,0,255)
Dim Shared As Integer morado2=RGB(170,0,170)
Dim Shared As Integer morado3=RGB(85,0,85)
'
Dim Shared As Integer gris=RGB(200,200,200)
Dim Shared As Integer gris2=RGB(120,120,120)
Dim Shared As Integer gris3=RGB(80,80,80)
Dim Shared As Integer gris4=RGB(30,30,30)

Dim Shared As Integer paleta(26)
paleta(0)=blanco
paleta(1)=negro 
paleta(2)=rojo  
paleta(3)=rojo2 
paleta(4)=rojo3  
paleta(5)=verde 
paleta(6)=verde2
paleta(7)=verde3 
paleta(8)=azul 
paleta(9)=azul2
paleta(10)=azul3 
paleta(11)=amarillo
paleta(12)=amarillo2
paleta(13)=amarillo3
paleta(14)=cian  
paleta(15)=cian2 
paleta(16)=cian3 
paleta(17)=morado
paleta(18)=morado2
paleta(19)=morado3
paleta(20)=cielo
paleta(21)=marron
paleta(22)=rosa
paleta(23)=gris  
paleta(24)=gris2 
paleta(25)=gris3 
paleta(26)=gris4 


Sub micolor(a As uInteger) ' debe ser uinteger, porque sino, los valores negativos dan colores falsos
	If a<27 Then Color paleta(a) Else Color a
End Sub