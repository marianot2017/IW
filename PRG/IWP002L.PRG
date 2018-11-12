*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ Sys.Info ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ
*)-H ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
*)-H Programa        : IWP002L.PRG -> Listador de clientes
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

*) Variables internas----------------------------------------------------------

w_pfile='02l'
w_prog =upper(prompt())
w_title=upper(prompt())
w_alta =.f.

*) Variables-------------------------------------------------------------------

w_orde=1
w_clde=space(5)
w_clha='99999'
w_node=space(15)
w_noha=repl('Z',15)
w_inci=.f.

w_drep=''
w_hrep=''

w_imco=imco()

*) Apertura de archivos--------------------------------------------------------

fieldvec[1]='F00'
fieldvec[2]='F01'
fieldvec[3]='F02'
fieldvec[4]='F04'
fieldvec[5]='F20'
fieldvec[6]='F01c'

use (d0f00) in 0 alias 'f00'						&&(Parametros)
use (d0f01) in 0 alias 'f01' order 'd1tip'   &&(Tablas)
use (d0f02) in 0 alias 'f02' order 'd2clie'  &&(Clientes)
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

=creatipos('C')
* use &d0f99.a in 0 again alias 'f01ti' 
use &d0f99.b in 0 again alias 'f01ti'
set order to tag clav in f01ti
*)-----------------------------------------------------------------------------

do ssm998.mpr
do iws002l.spr

pop key  
pop menu _msysmenu
=closall()
=borratipos()
dele file &d0f99.c.dbf
return

***vfp***
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons02l
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
	on key label f9 do cons02l0
	keyboard '{f9}'
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
proc cons02l0
*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
on key label f9
***vfp***

if .not. valid02l(.t.)
	wait wind 'Verifique los datos ingresados...'
   return 
endif

=gentmp()

if w_orde=1

	w_drep=w_clde
	w_hrep=w_clha
	
	if used('f99c')
		sele f99c
		use
		dele file &d0f99.c.dbf
	endif

	sele 0
	select *;
		from f02;
		where between(d2clie,w_clde,w_clha);
		into dbf &d0f99.c order by d2clie
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
		from f02;
		where between(d2nomb,w_node,w_noha);
		into dbf &d0f99.c order by d2nomb
	use &d0f99.c alias f99c exclusive
			
endif

if !p_cancela
	return
endif

do repdest &&(Setea variables internas del reporte)

sele f99c
if w_inci
	set relation to 'C'+d2clie into f20
	set skip to f20
endif
set relation to 'TD'+d2tido into f01t  additive
set relation to 'TI'+d2tipo into f01ti additive
set relation to 'JU'+d2juri into f01 additive
go top

if !empty(p_repclaus) &&(debe ser preview o to printer)
	p_report='iwr002l'
	=rep_form()
	p_report=''
endif

do represet &&(Resetea estado)

sele f99c
set relation to
return

*)อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ
FUNC valid02l
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
		show get w_clde enable
		show get w_clha enable
		w_node=space(15)
		w_noha=repl('Z',15)
		show get w_node disable
		show get w_noha disable
	else
		w_clde=space(5)
		w_clha='99999'
		show get w_clde disable
		show get w_clha disable
		show get w_node enable
		show get w_noha enable
	endif
endif

if w_vari='W_CLDE' or w_show
	w_clde=strcero(w_clde)
	show get w_clde
	if lastk()=p_f4
		do fhelp with 'W_CLDE','','iwp002','f02','d2clie','d2clie','',(p_strc),'w_retu'
	endif
endif

if w_vari='W_CLHA' or w_show
	w_clha=strcero(w_clha)
	show get w_clha
	if lastk()=p_f4
		do fhelp with 'W_CLHA','','iwp002','f02','d2clie','d2clie','',(p_strc),'w_retu'
	endif
	if w_clde>w_clha
		if !w_show
			wait wind 'Hasta debe ser mayor o igual que desde...'
		endif
		w_retu=.f.
	endif
endif

if w_vari='W_NODE' or w_show
	if lastk()=p_f4
		do fhelp with 'W_NODE','','iwp002','f02','d2clie','d2nomb','',(p_strc),'w_retu'
	endif
endif

if w_vari='W_NOHA' or w_show
	if lastk()=p_f4
		do fhelp with 'W_NOHA','','iwp002','f02','d2clie','d2nomb','',(p_strc),'w_retu'
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
report form iwr002l
