with Ada .Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;

package body Algebre is

	-- Initialise un vecteur avec une même coordonnée
	procedure Initialiser_Vecteur(V : in out T_Vecteur; Coordonnee : in float) is
	begin
		for i in 1..N loop
			V(i) := Coordonnee;
		end loop;
	end Initialiser_Vecteur;


	-- Initialise une matrice avec une même coordonnée
	procedure Initialiser_Matrice(M : in out T_Matrice; Coordonnee : in float) is
	begin
		for i in 1..N loop
			Initialiser_Vecteur(M(i), Coordonnee);
		end loop;
	end Initialiser_Matrice;


	-- Calcule et renvoie le produit d'un vecteur V et d'une matrice M
	function Produit_Vecteur_Matrice(V : in T_Vecteur; M : in T_Matrice) return T_Vecteur is
		P : T_Vecteur;
	begin
		for j in 1..N loop
			P(j) := 0.0;
			for k in 1..N loop
				P(j) := P(j) + V(k)*M(k)(j);
			end loop;
		end loop;
		return(P);
	end Produit_Vecteur_Matrice;

	-- Calcule et renvoie le produit d'un scalaire a et d'une matrice M
	procedure Produit_Scalaire_Matrice(a : in float; M : in out T_Matrice) is
	begin
		for i in 1..N loop
			for j in 1..N loop
				M(i)(j) := a*M(i)(j);
			end loop;
		end loop;
	end Produit_Scalaire_Matrice;


	-- Calcule et renvoie la somme de la ligne Ligne_i d'une matrice M
	function Somme_Ligne(M : in T_Matrice; Ligne_i : in Integer) return float is
		Somme : float := 0.0;
	begin
		for j in 1..N loop
			Somme := Somme + M(Ligne_i)(j);
		end loop;
		return(Somme);
	end Somme_Ligne;


	-- Calcule et renvoie la somme de deux matrices
	function Somme_Matrice(M1 : in T_Matrice; M2 : in T_Matrice) return T_Matrice is
		M : T_Matrice;
	begin
		for i in 1..N loop
			for j in 1..N loop
				M(i)(j) := M1(i)(j) + M2(i)(j);
			end loop;
		end loop;
		return(M);
	end Somme_Matrice;


	-- Verifie si la ligne i est vide
	function Ligne_Vide(M : in T_Matrice; Ligne_i : in Integer) return Boolean is
		Est_Vide : Boolean := True;
		j : Integer := 1;
	begin
		loop
			if M(Ligne_i)(j) /= 0.0 then
				Est_Vide := False;
			end if;
			j := j + 1;
		exit when (Est_Vide = False) or (j = N);
		end loop;
		return(Est_Vide);
	end Ligne_Vide;


	-- Normalise la ième ligne de M
	procedure Normaliser_Ligne(M : in out T_Matrice; Ligne_i : in Integer) is
		Norme : float;
	begin
		Norme := Somme_Ligne(M, Ligne_i);
		for j in 1..N loop
			M(Ligne_i)(j) := M(Ligne_i)(j) / Norme;
		end loop;
	end Normaliser_Ligne;
	
	-- Echange les valeurs de N1 et N2
	procedure Echanger(N1 : in out Float, N2 : in out Float)
		Memoire : Float;
	begin 
		Memoire := N1;
		N1 := N2;
		N2 := Memoire;
	end Echanger;

	-- Partitionner selon l'algorithme du tir rapide :
    function Partition(Poids : T_Vecteur; PageRank : T_Vecteur; debut : Integer; fin : Integer) return Integer is
        pivot : Float;
        i_pivot : Integer;
    begin
        -- Choix du pivot
        pivot := Poids(debut);
        i_pivot := debut;
        -- Permute tous les éléments de debut à fin, de sorte que :
        -- pour tout (k, k'), debut <=k < i_pivot <= k' < fin
        -- Poids(k) < pivot < Poids(k')
	-- On parcourt l'ensemble du tableau pour determiner le nombre d'elements
	-- plus petit que le pivot
        for k in debut+1..fin-1 loop
            if Poids(k) < pivot then
                i_pivot := i_pivot + 1;
	-- on "empile" les elements plus petit que le pivot en partant de l'indice de depart du pivot
                Echanger(Poids(k), Poids(i_pivot));
                Echanger(PageRank(k), PageRank(i_pivot));
            end if;
        end loop;
        Echanger(Poids(debut), Poids(i_pivot));
        Echanger(PageRank(debut), PageRank(i_pivot));
        return i_pivot;
    end Partition;

    -- Trier PageRank et les pages en fonction du PageRank
    procedure Trier(Poids : in out T_Vecteur; PageRank : in out T_Vecteur; debut : in Integer; fin : in Integer) is
        i_pivot : Integer;
    begin
        if debut < fin then
            i_pivot := Partition(Poids, PageRank, debut, i_pivot);
            Trier(Poids, PageRank, debut, i_pivot);
            Trier(Poids, PageRank, i_pivot+1, fin);
        end if;
    end Trier;


end Algebre;
