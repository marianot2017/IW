*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP003L.PRG -> Listador de proveedores
*)-H
*)-H Fecha de inicio :
*)-H Fecha de fin    :
*)-H
*)-H Updates
*)-H
*)-H Fecha       Programmer      Comentarios
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PRIVATE ALL LIKE w_*

*) Salvado de estados anteriores-----------------------------------------------

push key clear		  &&(Salva estados de teclas)
push menu _msysmenu &&(Salva estado de menu)
set skip of menu _msysmenu .t.

*) Variables-------------------------------------------------------------------

w_pfile='03l'
w_prog =upper(prompt())
w_title=upper(prompt())
w_alta =.f.

w_orde=1
w_prde=space(5)
w_prha='99999'
w_node=space(15)
w_noha=repl('Z',15)
w_inci=.f.

w_drep=''
w_hrep=''

w_imco=imco()

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F03'
fieldvec[4]='F04'
fieldvec[5]='F20'
fieldvec[6]='F01c'

use (d0f00) in 0 alias 'f00'						&&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'   &&(Tablas)
use (d0f03) in 0 alias 'f03' order 'd3prov'  &&(Proveedores)
use (d0f04) in 0 alias 'f04' order 'd4peri'  &&(Periodos)
use (d0f20) in 0 alias 'f20' order 'd20tipo' &&(Tipos impos. x periodos)
if w_imco
	ncf01=w0fipre+'01'
	use (ncf01) in 0 alias f01c order tag 'd1cuen'
endif

if neterr()
	pop key
	pop menu _msysmenu
	fieldvec=''
	=closall()
	return
endif
fieldvec=''

=creatipos('P')

* use &d0f99.a in 0 again alias 'f01ti'
use &d0f99.b in 0 again alias 'f01ti'

set order to tag clav in f01ti
*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws003l.spr

pop key
pop menu _msysmenu
=closall()
=borratipos()

dele file &d0f99.c.dbf

return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons03l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons03l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons03l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid03l(.t.)
	wait wind 'Verifique los datos ingresados...'
	return
endif

=gentmp()

if w_orde=1

	w_drep=w_prde
	w_hrep=w_prha

	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif

	sele 0
	select *;
		from f03;
		where between(d3prov,w_prde,w_prha);
		into dbf &d0f99.c order by d3prov
	use &d0f99.c alias f99c exclusive

else

	w_drep=w_node
	w_hrep=w_noha

	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif

	sele 0
	select *;
		from f03;
		where between(d3nomb,w_node,w_noha);
		into dbf &d0f99.c order by d3nomb
	use &d0f99.c alias f99c exclusive

endif

if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
if w_inci
	set relation to 'P'+d3prov into f20
	set skip to f20
endif
set relation to 'TD'+d3tido into f01t  additive
set relation to 'TI'+d3tipo into f01ti additive
set relation to 'JU'+d3juri into f01 additive
go top

if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr003l'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

sele f99c
set relation to
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid03l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
PARAMETERS w_show

if empty(parameters())
	w_show=.f.
endif

PRIVATE w_retu,w_vari
w_retu=.t.
w_vari=varread()

if w_vari='W_ORDE'
	if w_orde=1
		show get w_prde enable
		show get w_prha enable
		w_node=space(15)
		w_noha=repl('Z',15)
		show get w_node disable
		show get w_noha disable
	else
		w_prde=space(5)
		w_prha='99999'
		show get w_prde disable
		show get w_prha disable
		show get w_node enable
		show get w_noha enable
	endif
endif

if w_vari='W_PRDE' or w_show
	w_prde=strcero(w_prde)
	show get w_prde
	if lastk()=p_f4
		do fhelp with 'W_PRDE','','iwp003','f03','d3prov','d3prov','',(p_strc),'w_retu'
	endif
endif

if w_vari='W_PRHA' or w_show
	w_prha=strcero(w_prha)
	show get w_prha
	if lastk()=p_f4
		do fhelp with 'W_PRHA','','iwp003','f03','d3prov','d3prov','',(p_strc),'w_retu'
	endif
	if w_prde>w_prha
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='W_NODE' or w_show
	if lastk()=p_f4
		do fhelp with 'W_NODE','','iwp003','f03','d3prov','d3nomb','',(p_strc),'w_retu'
	endif
endif

if w_vari='W_NOHA' or w_show
	if lastk()=p_f4
		do fhelp with 'W_NOHA','','iwp003','f03','d3prov','d3nomb','',(p_strc),'w_retu'
	endif
	if w_node>w_noha
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='W_INCI'
	=mchkbox(@w_inci)
endif
return(w_retu)

***puesto adrede para que incluya el report al proyecto**
report form iwr003l
