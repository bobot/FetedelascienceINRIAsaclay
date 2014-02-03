  open Robot
  let () = 
while
  couleur_est (blanc)
  || couleur_est (noir)
do
  while
    couleur_est (blanc)
  do
    avance_un_peu ()
  done;
  cherche_non_noir ()
done