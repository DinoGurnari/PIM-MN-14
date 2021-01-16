with Ada .Text_IO;          use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body Algebre is

	-- Initialise un vecteur avec une même coordonnée
	procedure Initialiser_Vecteur(V : in out T_Vecteur; Coordonnee : in T_Element) is
	begin
		for i in 1..N loop
			V(i) := Coordonnee;
		end loop;
	end Initialiser_Vecteur;


	-- Initialise une matrice avec une même coordonnée
	procedure Initialiser_Matrice(M : in out T_Matrice; Coordonnee : in T_Element) is
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
	procedure Produit_Scalaire_Matrice(a : in T_Element; M : in out T_Matrice) is
	begin
		for i in 1..N loop
			for j in 1..N loop
				M(i)(j) := a*M(i)(j);
			end loop;
		end loop;
	end Produit_Scalaire_Matrice;


	-- Calcule et renvoie la somme de la ligne Ligne_i d'une matrice M
	function Somme_Ligne(M : in T_Matrice; Ligne_i : in Integer) return T_Element is
		Somme : T_Element := 0.0;
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
		Norme : T_Element;
	begin
		Norme := Somme_Ligne(M, Ligne_i);
		for j in 1..N loop
			M(Ligne_i)(j) := M(Ligne_i)(j) / Norme;
		end loop;
	end Normaliser_Ligne;
	
	-- Echange les valeurs de N1 et N2
	procedure Echanger(N1 : in out T_Element; N2 : in out T_Element) is
		Memoire : T_Element;
	begin 
		Memoire := N1;
		N1 := N2;
		N2 := Memoire;
	end Echanger;

	-- Partitionner selon l'algorithme du tir rapide :
    function Partition(Poids : T_Vecteur; PageRank : T_Vecteur; debut : Integer; fin : Integer) return Integer is
        pivot : T_Element;
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
                --Echanger(Poids(k), Poids(i_pivot));
                --Echanger(PageRank(k), PageRank(i_pivot));
            end if;
        end loop;
        --Echanger(Poids(debut), Poids(i_pivot));
        --Echanger(PageRank(debut), PageRank(i_pivot));
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
	
		
	procedure Free is
		new Ada.Unchecked_Deallocation (Object => T_Cellule, Name => T_LCA);

	-- Initialiser une Sda.  La Sda est vide.
	procedure Initialiser(Sda: out T_LCA) is
	begin
		Sda := Null;
	end Initialiser;

	-- Est-ce qu'une Sda est vide ?
	function Est_Vide (Sda : T_LCA) return Boolean is 
	begin
        return Sda = Null;
	end Est_Vide;
		
	-- Enregistrer une Donnée associée à une Colonne dans une Sda.
	-- Si la Colonne est déjà présente dans la Sda, sa donnée est changée.
	procedure Enregistrer (Sda : in out T_LCA ; Colonne : in Integer ; Donnee : in T_Element) is
		LCA0 : T_LCA := Sda;
		LCA1 : T_LCA;
		Est_Enregistre : Boolean := False;
	begin
		if Est_Vide(Sda) then
			Sda := new T_Cellule;
			Sda.all.Colonne := Colonne;
			Sda.all.Donnee := Donnee;
			Initialiser(Sda.all.Suivant);
			Est_Enregistre := True;
		end if;
		-- On parcourt la liste
		while not Est_Enregistre loop
			-- Si la clef est présente on la modifie
			if LCA0.all.Colonne = Colonne then
				LCA0.all.Donnee := Donnee;
				Est_Enregistre := True;
			-- On va à la dernière cellule ou à la cellule qui contient déjà la clef
			elsif LCA0.all.Suivant = null then
				LCA1 := new T_Cellule;
				LCA1.all.Colonne := Colonne;
				LCA1.all.Donnee := Donnee;
				Initialiser(LCA1.all.Suivant);
				LCA0.all.Suivant := LCA1;
				Est_Enregistre := True;
			end if;
			LCA0 := LCA0.all.Suivant;
		end loop;
	end Enregistrer;
		
	-- Obtenir la donnée associée à une Colonne dans la Sda.
	-- Exception : Cle_Absente_Exception si Colonne n'est pas utilisée dans l'Sda
	function La_Donnee (Sda : in T_LCA ; Colonne : in Integer) return T_Element is
		LCA0 : T_LCA := Sda;
		Resultat : T_Element;
		Est_Trouvee : Boolean := False;
	begin
		-- On cherche la clef
		while not Est_Trouvee and not Est_Vide(LCA0) loop
			if LCA0.all.Colonne = Colonne then
				Resultat := LCA0.all.Donnee;
				Est_Trouvee := True;
			else
				LCA0 := LCA0.all.Suivant;
			end if;
		end loop;
		return Resultat;
	end La_Donnee;

	-- Supprimer la Donnée associée à une Colonne dans une Sda.
	-- Exception : Cle_Absente_Exception si Colonne n'est pas utilisée dans la Sda
	procedure Supprimer (Sda : in out T_LCA ; Colonne : in Integer) is
		LCA0 : T_LCA := Sda;
		LCA1 : T_LCA := LCA0.all.Suivant;
	begin
		-- On regarde si c'est la première cellule
		if LCA0.all.Colonne = Colonne then
			-- S c'est le cas on libère la première cellule
	    		Sda := LCA1;
			Free(LCA0);
		else
			-- Sinon on cherche la clef
			while LCA1.all.Colonne /= Colonne loop
				LCA0 := LCA0.all.Suivant;
				LCA1 := LCA1.all.Suivant;
			end loop;
			-- On raccorde la cellule n-1 à la cellule n+1 et on libère n
			if LCA1.all.Colonne = Colonne then
				LCA0.all.Suivant := LCA1.all.Suivant;
				Free(LCA1);
			end if;
	    	end if;
	end Supprimer;
		
	-- Supprimer tous les éléments d'une Sda.
	procedure Vider (Sda : in out T_LCA) is
	begin
		-- On supprime toutes les cellules une par une
		while Sda /= Null loop
			Supprimer(Sda, Sda.all.Colonne);
		end loop;
	end Vider;

	-- Initialise une matrice creuse
	procedure Initialiser_Matrice_Creuse(M : in out T_Matrice_Creuse) is
	begin
		for i in 1..N loop
			Initialiser(M(i));
		end loop;
	end Initialiser_Matrice_Creuse;

	-- Calcule et renvoie le produit d'un vecteur V et d'une matrice creuse M
	function Produit_Vecteur_Matrice_Creuse(V : in T_Vecteur; M : in T_Matrice_Creuse; alpha : in Float) return T_vecteur is
		P : T_Vecteur;
		Ligne : T_LCA;
		K : Integer;
		val : T_Element := T_Element((1.0 - alpha)/Float(N));
	begin
		Initialiser_Vecteur(P, 0.0);
		for i in 1..N loop
			Ligne := M(i);
			if Ligne = Null then
				for k in 1..N loop
					P(k) := P(k) + V(i)*T_Element(1/N);
				end loop;
			else
				for k in 1..N loop
					P(k) := P(k) + V(i)*val;
				end loop;
				while Ligne /= Null loop
					K := Ligne.all.Colonne;
					P(K) := P(K) + V(i)*Ligne.all.Donnee*T_Element(alpha);
					Ligne := Ligne.all.Suivant;
				end loop;
			end if;
		end loop;
		return(P);
	end Produit_Vecteur_Matrice_Creuse;

	-- Calcule et renvoie le produit d'un scalaire a et d'une matrice creuse M
	procedure Produit_Scalaire_Matrice_Creuse(a : in T_Element; M : in out T_Matrice_Creuse) is
		Ligne : T_LCA;
	begin
		for i in 1..N loop
			Ligne := M(i);
			while Ligne /= Null loop
				Ligne.all.Donnee := Ligne.all.Donnee * a;
				Ligne := Ligne.all.Suivant;
			end loop;
		end loop;
	end Produit_Scalaire_Matrice_Creuse;

	-- Calcule et renvoie la somme de la ligne Ligne_i d'une matrice creuse M
	function Somme_Ligne_Lca(M : in T_Matrice_Creuse; Ligne_i : in Integer) return T_Element is
		Somme : T_Element := 0.0;
		Ligne : T_LCA := M(Ligne_i);
	begin
		while Ligne /= Null loop
			Somme := Somme + Ligne.all.Donnee;
			Ligne := Ligne.all.Suivant;
		end loop;
		return(Somme);
	end Somme_Ligne_Lca;
	
	-- Normalise la ième ligne de M
	procedure Normaliser_Ligne_Lca(M : in out T_Matrice_Creuse; Ligne_i : in Integer) is
		Norme : T_Element;
		Ligne : T_LCA := M(Ligne_i);
	begin
		Norme := Somme_Ligne_Lca(M, Ligne_i);
		while Ligne /= Null loop
			Ligne.all.Donnee := Ligne.all.Donnee / Norme;
			Ligne := Ligne.all.Suivant;
		end loop;
	end Normaliser_Ligne_Lca;	
	
	-- Supprimer tous les éléments d'une Matrice creuse.
	procedure Vider_Matrice (M : in out T_Matrice_Creuse) is
	begin
		for i in 1..N loop
			Vider(M(i));
		end loop;
	end Vider_Matrice;
				
end Algebre;
