*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H Programa        : iwp010i.prg -> Copiado de empresas
*-H Release			: 01
*-H Fecha de inicio :
*-H Fecha de fin    :
*-H
*-H Actualizaciones
*-H
*-H  Fecha       Programmer      Comentarios
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
private all like w_*

** Salvado de estados anteriores

*push key clear      && Salva estados de teclas
*push menu _msysmenu && Salva estado de menu

w_curobj=_curobj     && Salva numero de objeto en el read activo
w_alan=alias()       && Salva alias vigente al llamado del programa
w_wind=wontop()      && Salva ventana vigente al llamado del programa
w_rdlevel=rdlevel()  && Salva Nro. de read activo

** Variables internas

w_prog=upper(prompt())

w_alta=.f.
w_agre=.not. mprot(p_user,w_prog,'Registro','Agregar')
w_modi=.not. mprot(p_user,w_prog,'Registro','Modificar')
w_recn=0

** Datos del archivo a mantener
w_pfile='10i'
w_clave='P0COEM'
w_alias='F10'
w_title=db_name(w_alias)

w_empr=1
d0audi='N'

fieldvec[1] = 'F10'
fieldvec[2] = 'F12'
fieldvec[3] = 'F66'

use &d0f10 in 0 alias f10 order p0coem excl
use &d0f12 in 0 alias f12 exclu
use &d0f66 in 0 alias f66 order d66user excl

if neterr()

	*     pop key
	*     pop menu _msysmenu

	if .not. empty(w_wind)
		acti wind (w_wind)
	endif

	=closall()

	return

endif

fieldvec=''

w_title=space(44)+'TOMA EMPRESAS DE IVA DOS'

modi wind screen;
	title w_title ;
	fill file memofond.bmp icon file iw.ico

zoom wind screen max

do cons10i

w_title=''

modi wind screen;
	title w_title ;
	fill file memofond.bmp icon file iw.ico

zoom wind screen max


*pop key
*pop menu _msysmenu

if wexist(w_wind) and !empty(w_wind)
	acti wind (w_wind)
endif

=closall()

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons10i
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons10i0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons10i0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

sele (w_alias)
go top

*if .not. eof('f10')  &&Hay empresas en el sistema
*   return
*endif

** Si esta la empresa generica y no existe la version de clipper de iva

if !mfile(d0disc+'\iv\ivf010.dbf')
	=advgrave('No existe sistema de IVA DOS')
	return
endif

if !dispok(d0disc+'\iv')
	=advgrave('No se puede acceder al disco '+d0disc+', del sistema de IVA DOS')
	return
endif

** Si existe iva tomo empresas de iva\clipper

fieldvec[1] = 'c10 Empresas DOS'

use &d0disc\iv\ivf010 in 0 alias c10

if neterr()

	fieldvec=''
	return

endif

fieldvec=''

go top in c10

fieldvec[1] = 'C77 Semแforo DOS'

use &d0disc\iv\ivf077 in 0 alias c77 exclu

if neterr()

	fieldvec=''
	return

endif

fieldvec=''

fieldvec[1] = 'f77 Semแforo'

=set_alia('f77')
use &d0f77 alias f77 exclusive

if neterr()

	do while .t.

		fieldvec[1] = 'f77 Semแforo'

		use &d0f77 alias f77

		if !neterr()
			exit
		endif
	enddo

	=rest_alia()

	fieldvec=''
	return

endif

fieldvec=''
=rest_alia()

sele c10

repl all p0sele with ' '

*** No toma las empresas existentes en windows, excepto la empresa 00 ***
set relation to p0coem into f10
set filter to (p0coem<>f10.p0coem or p0coem='00') and p0vees+p0veda>='301301'

go top

if eof()
	w_err1='No se detectaron empresas en IVA DOS'
	=advgrave(w_err1)
	return
endif

w_opc=0

do iws010i.spr && Screen de eleccion de empresas

sele f12
replace f12.p0toco with 'N'
d0toco='N'

sele c10

count for c10.p0sele='>' to w_cant

if w_opc=6	&& Incorpora

	if w_cant=1 && una empresa

		if sino('Desea incorporarla ?','No', 'Toma empresa de IVA DOS')='N'
			return
		endif

	else

		if sino('Desea incorporarlas ?','No', 'Toma empresas de IVA DOS')='N'
			return
		endif

	endif

else

	return

endif

dime w_file(1)
w_cant=adir(w_file,'fi\00\*.dbf')

for w_i=1 to w_cant

	do cpzero with ('fi\00\'+w_file(w_i,1)), (850)

endfor

dime w_file(1)
w_cant=adir(w_file,'fi\ivf*.dbf')

for w_i=1 to w_cant

	do cpzero with ('fi\'+w_file(w_i,1)), (850)

endfor

do cpzero with ('fi\fex.dbf'), (850)

go top in c10

do while .not. eof('c10')

	w_badfile=.f.

	if c10.p0sele=' '
		skip in c10
		loop
	endif

	wait 'Importando archivos Empresa : '+left(c10.p0empr,20) wind nowait
	w_path=d0disc+alltrim(c10.p0path)
	w_resu=adir(w_file,w_path+'*.dbf')
	w_subd=alltrim(c10.p0coem)

	if w_resu>0

		if c10.p0coem='00'	&& caso especial

			if sino('Reemplaza la Empresa 00 WINDOWS, por la Empresa 00 DOS', 'NO', 'Toma empresa de Iva DOS')='N'
				skip in c10
				loop
			else
				=deledir(w_subd,'D')
			endif

		endif

		if mkdir(w_subd)<>0
			w_err1='No se puede copiar al directorio \IW\'+w_subd
			=advsuave(w_err1)
			skip in c10
			loop
		endif

		for w_repe=1 to alen(w_file,1)

			w_arch=w_path+alltrim(w_file[w_repe,1])

			w_orig='\IW\FI\00\'+alltrim(w_file[w_repe,1])

			if mfile(w_orig)	&& copia dbf

				w_dest='\IW\'+c10.p0coem+'\'+alltrim(w_file[w_repe,1])

				copy file &w_orig to &w_dest

			else

				loop

			endif

			w_orig=left(w_orig,len(w_orig)-3)+'fpt'

			if mfile(w_orig)	&& copia fpt

				w_dest=left(w_dest,len(w_dest)-3)+'fpt'

				copy file &w_orig to &w_dest

				w_dest=left(w_dest,len(w_dest)-3)+'dbf'

			endif

			fieldvec[1]='f99 '+w_dest

			use &w_dest alias f99 in 0

			if neterr()
				=audi('TOM S','Archivo Da๑ado','WIN:'+justfname(w_dest),w_subd)
				w_badfile=.t.
			else

				sele f99

				=ncodpage()	&& no transforma los campos que vienen de clipper

				fieldvec[1]='f99 '+w_arch

				append from &w_arch

				if neterr()
					=audi('TOM S','Archivo Da๑ado','DOS:'+justfname(w_arch),w_subd)
					w_badfile=.t.
				endif

				use

			endif

		endfor

		if !used('s01')
			do while .t.

				fieldvec[1]='s01 Usuarios'

				use ssf001 in 0 alias s01 order 1

				if !neterr()
					exit
				endif
			enddo
		endif

		sele s01
		go top

		do while !eof()

			if d1user='SUPERVISOR'
				skip
				loop
			endif

			m.d1user=d1user

			=seek(m.d1user+c10.p0coem, 'f66')

			if !found('f66')

				=net_appe('f66') && Autorizacion de usuario/empresa

			endif

			=set_alia('f66')
			repl d66user with m.d1user, d66empr with c10.p0coem
			=rest_alia()

			skip

		enddo

		** Si copio el subdirectorio de la empresa la adiciono en el f10

		=set_alia('c10')
		scatter memvar
		=rest_alia()

		if c10.p0coem='00'	&& caso especial

			=seek(c10.p0coem, 'f10')

			if !found('f10')	&& no deberia pasar

				=net_appe('f10')

			endif

		else

			=net_appe('f10') && Empresas

		endif

		d0f00='\IW\'+c10.p0coem+'\ivf000'
		use (d0f00) in 0 alias 'f00' EXCLUSIVE		&&(Parametros)
		sele f00
		replace p0emcg with ''
		use

		=set_alia('f10')
		m.p0empr=cpconvert(850,1252,m.p0empr)
		gather memvar
		repl f10.p0path with '\IW\'+c10.p0coem+'\'		&& arreglo el path nuevo
		
		if w_badfile
			replace f10.p0vein with '999', f10.p0vees with '999', f10.p0veda with '999'
			=rest_alia()
		else
			=rest_alia()
			=audi('TOM S','Exitosa','',w_subd)
		endif



	endif

	skip in c10

enddo


wait clear

** Adiciona empresas de iva a IW
sele c10
repl all p0sele with ' '
use
sele f10

do civf10i      && Copia el contenido de las bases ivf0?? de \iv\

do db_rein10i  && Genera indice

dime w_file(1)
w_cant=adir(w_file,'fi\00\*.dbf')

for w_i=1 to w_cant

	do cpzero with ('fi\00\'+w_file(w_i,1)), (1252)

endfor

dime w_file(1)
w_cant=adir(w_file,'fi\ivf*.dbf')

for w_i=1 to w_cant

	do cpzero with ('fi\'+w_file(w_i,1)), (1252)

endfor

do cpzero with ('fi\fex.dbf'), (1252)

return

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
procedure when10i
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_show
private w_retu,w_vari

w_retu=.t.
w_vari=varread()

** Eleccion de empresas              ************************

if w_vari='W_EMPR'

	do case

	case lastkey()=110	&& No incorporar

		w_opc=1

		clear read

	case lastkey()=116	&& marcar Todas

		w_opc=2

		=set_alia('c10')
		repl all c10.p0sele with '>'
		=rest_alia()
		=advsuave('Empresa 00 existente en WINDOWS')

	case lastkey()=114	&& desmarcaR todas

		w_opc=3

		=set_alia('c10')
		repl all c10.p0sele with ' '
		=rest_alia()

	case lastkey()=109	&& Marcar

		w_opc=4

		if c10.p0sele=' '
			=set_alia('c10')
			repl c10.p0sele with '>'
			=rest_alia()
			if c10.p0coem='00'
				=advsuave('Empresa 00 existente en WINDOWS')
			endif
		endif

	case lastkey()=100	&& Desmarcar

		w_opc=5

		if c10.p0sele='>'
			=set_alia('c10')
			repl c10.p0sele with ' '
			=rest_alia()
		endif

	case lastkey()=105	&& Incorporar

		w_opc=6

		clear read

	endcase

	show get w_empr

endif

if c10.p0sele='>'
	show get w_opc,4 disable
	show get w_opc,5 enable
else
	show get w_opc,4 enable
	show get w_opc,5 disable
endif

w_recno=recno()

count for c10.p0sele='>' to w_cant

if w_cant=0
	show get w_opc,6 disable
else
	show get w_opc,6 enable
endif

go w_recno

return


* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
procedure valid10i
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
parameters w_show
private w_retu,w_vari

w_retu=.t.
w_vari=varread()

if w_vari='W_OPC'

	do case

	case w_opc=1	&& No incorporar

		clear read

	case w_opc=2	&& marcar Todas

		=set_alia('c10')
		repl all c10.p0sele with '>'
		=rest_alia()

		=advsuave('Empresa 00 existente en WINDOWS')

	case w_opc=3	&& desmarcaR todas

		=set_alia('c10')
		repl all c10.p0sele with ' '
		=rest_alia()

	case w_opc=4	&& Marcar

		if c10.p0sele=' '
			=set_alia('c10')
			repl c10.p0sele with '>'
			=rest_alia()
			if c10.p0coem='00'
				=advsuave('Empresa 00 existente en WINDOWS')
			endif
		endif

	case w_opc=5	&& Desmarcar

		if c10.p0sele='>'
			=set_alia('c10')
			repl c10.p0sele with ' '
			=rest_alia()
		endif

	case w_opc=6	&& Incorporar

		clear read

	endcase

	show get w_empr

	show get w_opc

endif

return

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
procedure db_rein10i
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

sele f10
use
sele f66
use

=db_indexiw(.t.,'F10')

=db_indexiw(.t.,'F66')

=closall()

return

* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
procedure civf10i
* อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ

* archivo de control de gabacion del fex desde DOS
if !mfile('fexyffo.win') and mfile(d0disc+'\iv\fex.dbf') and mfile(d0disc+'\iv\ffo.dbf') 
		
	if sino('Toma modelos de impresi๓n de comprobantes (fex.dbf y ffo.dbf) de IVA DOS','S','Toma empresa de Iva DOS')='S'

		w_orig=d0disc+'\iv\ffo.dbf'
		w_dest='ffo'
		do while .t.
			fieldvec[1]='ffo'
			use &w_dest in 0  alias ffo exclusive
			if !neterr()
				exit
			endif
		enddo
		sele (w_dest)
		zap
		append from &w_orig
		use
		
		w_orig=d0disc+'\iv\fex.dbf'
		w_dest='fex'
		do while .t.
			fieldvec[1]='fex'
			use &w_dest in 0 alias fex exclusive
			if !neterr()
				exit
			endif
		enddo
		sele (w_dest)
		zap
		append from &w_orig
		use
		
		sele (w_alias)
	else
		w_orig=''
		w_dest=''
	endif		
	w_file='fexyffo.win'
	w_hand=f_create(w_file)
	if w_hand > 0
		=f_write(w_hand,dtoc(date())+' '+time())
		=f_write( w_hand,iif(empty(w_orig), ' No copio de IVA DOS', ' Copio desde '+w_orig) )
		=f_close(w_hand)
	endif
endif

*if sino('Toma de IVA DOS la cabecera y detalle de informes definidos','N','Toma empresa de Iva DOS')='S'

	if mfile(d0disc+'\iv\ivf016.dbf') and mfile(d0disc+'\iv\ivf017.dbf')

		sele 0 
		use fi\ivf016.dbf alias f16 exclu
		append from &d0f16..dbf
		index on d16defi tag d16defi

		sele 0 
		use fi\ivf017.dbf alias f17 exclu
		append from &d0f17..dbf
		index on d17defi+d17orde tag d17defi

		do while .t.

*			fieldvec[1]='F16'
			fieldvec[2]='F16x'
			d0f16x=d0disc+'\iv\ivf016.dbf'
*			fieldvec[3]='F17'

*			use &d0f16  in 0 alias f16 order d16defi exclu
			use &d0f16x in 0 alias f16x
*			use &d0f17  in 0 alias f17 order d17defi exclu

			if !neterr()
				exit
			endif

		enddo
		fieldvec=''

		sele f16
		=ncodpage()	&& no transforma los campos que vienen de clipper
		sele f16x
		go top
		do while !eof('f16x')
			scatter memvar
			if !seek(m.d16defi,'f16')	&&Inf. no existe en windows agrega el de DOS
				sele f16
				append blank
				gather memvar
				sele f17
				=ncodpage()	&& no transforma los campos que vienen de clipper
				appe from &d0disc\iv\ivf017 for d17defi==m.d16defi
				sele f16x
*			else
*				if sino('Reemplaza el informe definido '+m.d16defi+' '+alltri(m.d16nomb), 'N','Toma empresa de Iva DOS')='S'
*					sele f16
*					gather memvar
*					sele f17
*					delete for d17defi==m.d16defi
*					=ncodpage()	&& no transforma los campos que vienen de clipper
*					appe from &d0disc\iv\ivf017 for d17defi==m.d16defi
*					sele f16x
*				endif
			endif
			skip in f16x
		enddo
		sele f16
		&& R.01b Mariano
*		copy to &d0f16..dbf
		***vfp***
		IF version()='Visual FoxPro'
			copy to &d0f16..dbf type fox2x
		else
			copy to &d0f16..dbf
		endif
		&& R.01e Mariano

		zap
		delete tag d16defi
		use
		sele f16x
		use
		release d0f16x
		sele f17
		&& R.01b Mariano
*		copy to &d0f17..dbf
		***vfp***
		IF version()='Visual FoxPro'
			copy to &d0f17..dbf type fox2x
		else
			copy to &d0f17..dbf
		endif
		&& R.01e Mariano
		zap
		delete tag d17defi
		use
	endif

*endif

**Aparea las tablas de sistema**
if mfile(d0disc+'\iv\ivf021.dbf')
	sele 0 
	use fi\ivf021.dbf alias f21 exclu
	append from &d0f21..dbf
	index on d21tip+d21cla tag d21tip

	do while .t.

*		fieldvec[1]='F21'
		fieldvec[2]='F21x'
		d0f21x=d0disc+'\iv\ivf021.dbf'
	
*		use &d0f21  in 0 alias f21  order 'd21tip' exclu
		use &d0f21x in 0 alias f21x 
	
		if !neterr()
			exit
		endif

	enddo

	fieldvec=''

	sele f21
	=ncodpage()	&& no transforma los campos que vienen de clipper
	sele f21x
	go top
	do while !eof('f21x')
		scatter memvar
		if m.d21tip='TI'
			m.d21tip='GC'
		endif
		if !seek(m.d21tip+m.d21cla,'f21')
			sele f21
			append blank
			gather memvar
			sele f21x
		endif
		skip in f21x
	enddo
	sele f21
	copy to &d0f21..dbf
	&& R.01b Mariano
*	copy to &d0f21..dbf
	***vfp***
	IF version()='Visual FoxPro'
		copy to &d0f21..dbf type fox2x
	else
		copy to &d0f21..dbf
	endif
	&& R.01e Mariano
	zap
	delete tag d21tip
	use
	sele f21x
	use
	release d0f21x
endif

**Empresa de conta en el IVA (99)**

d0f00x='co\99\c990000.dbf'
d0f01x='co\99\c990001.dbf'
d0f00y=d0disc+'\iv\co\99\c999100.dbf'
d0f01y=d0disc+'\iv\co\99\c999101.dbf'

if mfile(d0f00x) and mfile(d0f01x)

	do while .t.

		fieldvec[1]='F00x'
		fieldvec[2]='F01x'

		use &d0f00x in 0 alias f00x exclu
		use &d0f01x in 0 alias f01x exclu

		if !neterr()
			exit
		endif

	enddo

	fieldvec=''

	sele f00x

	if mfile(d0f00y) and reccount('f01x')=0
		zap
		=ncodpage()	&& no transforma los campos que vienen de clipper
		appe from &d0f00y
	endif

	use

	sele f01x

	if mfile(d0f01y) and reccount('f01x')=0
		zap
		=ncodpage()	&& no transforma los campos que vienen de clipper
		appe from &d0f01y
	endif
	use

endif

return

