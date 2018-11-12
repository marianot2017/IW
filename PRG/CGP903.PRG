*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : CGP903.PRG -> Def. de Estructura de ctas.contables
*)-H
*)-H Fecha de inicio : 22/12/1998.-
*)-H Fecha de fin    : 29/12/1998.-
*)-H
*)-H Actualizaciones
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE ALL LIKE w_*

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)

w_curobj=_curobj	  &&(Salva numero de objeto en el read activo)
w_alan=alias()		  &&(Salva alias vigente al llamado del programa)
w_wind=wontop()	  &&(Salva ventana vigente al llamado del programa)
w_rdlevel=rdlevel() &&(Salva Nro. de read activo)

*) Variables internas----------------------------------------------------------

w_prog=upper(prompt())
w_alta=.f.
w_agre=.not. mprot(p_user,w_prog,'Registro','Agregar')
w_modi=.not. mprot(p_user,w_prog,'Registro','Modificar')
w_recn=0
w_modicampo=.f.

*) Datos del archivo a mantener------------------------------------------------

w_pref =''
w_pfile='903'
w_alias='f00c'
w_title='PARAMETROS DE CUENTAS CONTABLES'

w_oldp=d0pict &&(en caso en que se anule la modificacion)
w_oldt=d0picp

*) Apertura de archivos--------------------------------------------------------

ncf00=w0fipre+'00'
ncf01=w0fipre+'01'

fieldvec[1]='F00C'
fieldvec[2]='F01C'

use (ncf00) in 0 alias 'f00c' EXCLUSIVE		&&(Parametros)
use (ncf01) in 0 alias 'f01c' order 'd1cuen' &&(Ctas.ctbles)

if neterr()
	pop key
	pop menu _msysmenu	
	fieldvec=''
   =closall()
   return
endif

fieldvec=''

*)-----------------------------------------------------------------------------

w_ctas=eof('f01c')

select (w_alias)
go top
scatter memvar
=work903()

set skip of menu _msysmenu .t.
do ssm999.mpr	&&(menu registro)

=skipbar('registro','all',.t.)
=skipbar('registro','Agregar'   ,.f.)
=skipbar('registro','Modificar',.f.)
=skipbar('registro','Calculadora',.f.)
=skipbar('registro','Salir',.f.)

on key label f1 do nada
if w_agre
	on key label f9 keyboard '{ctrl+f9}'
endif

do cgs903.spr

pop key 
pop menu _msysmenu
=closall()
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC wread903
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !w_ctas
	show get m.p0niv1 disable
	show get m.p0niv2 disable
	show get m.p0niv3 disable
	show get m.p0niv4 disable
	show get m.p0niv5 disable
	show get m.p0niv6 disable
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC actu903
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
if !valid903(.t.)
	return
endif

if begintran()
	wait wind 'Actualizando parแmetros...' time .4
	
	sele (w_alias)
	repl p0niv1 with m.p0niv1;
		,p0niv2 with m.p0niv2+m.p0niv1;
		,p0niv3 with m.p0niv3+m.p0niv2+m.p0niv1;
		,p0niv4 with m.p0niv4+m.p0niv3+m.p0niv2+m.p0niv1;
		,p0niv5 with m.p0niv5+m.p0niv4+m.p0niv3+m.p0niv2+m.p0niv1;
		,p0niv6 with m.p0niv6+m.p0niv5+m.p0niv4+m.p0niv3+m.p0niv2+m.p0niv1;
		,p0pict with m.p0pict
		
	go (recno()) in (w_alias)
	
	d0niv1=p0niv1
	d0niv2=p0niv2
	d0niv3=p0niv3
	d0niv4=p0niv4
	d0niv5=p0niv5
	d0niv6=p0niv6
	
	d0pict=alltrim(m.p0pict)
	d0picp=''
	for e=1 to len(d0pict)
		if subs(d0pict,e,1)='9'
			d0picp=d0picp+'9'
		else
			d0picp=d0picp+'.'
		endif
	next
	d0picp=d0picp+'.'

	
	=endtran()
	w_modicampo=.f.
	clear read
endif
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid903
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

w_retu=.t.
w_vari=varread()
w_vread='m.'+w_vari

s=7
for e=1 to 6
	x=str(e,1)
   if m.p0niv&x=0
   	s=e
      e=7
	endif
next

for e=s to 6
	x=str(e,1)
   if m.p0niv&x<>0
   	wait wind 'Definicion incorrecta de niveles...'
      return(.f.)
   endif
next

w_nuev=0
for e=1 to 20
	if substr(m.p0pict,e,1)='9'
   	w_nuev=w_nuev+1
   endif
next

w_cifr=m.p0niv1+m.p0niv2+m.p0niv3+m.p0niv4+m.p0niv5+m.p0niv6
if w_nuev<>w_cifr
	wait wind 'No coincide la imagen con los niveles...'
	return(.f.)
endif

=modicampo()


return(.t.)

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PROC work903
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
m.p0niv1=d0niv1
m.p0niv2=d0niv2-d0niv1
m.p0niv3=d0niv3-d0niv2
m.p0niv4=d0niv4-d0niv3
m.p0niv5=d0niv5-d0niv4
m.p0niv6=d0niv6-d0niv5
m.p0pict=subs(d0pict+spac(20),1,20)
return
