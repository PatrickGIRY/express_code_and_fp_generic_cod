# Le dilemne entre code expressif et code générique FP

Utiliser au mieux la plomberie fournie par le langage de programmation, ou exprimer au mieux le domaine métier ? C'est le dilemne habituel dans un langage de programmation tel que Java.

Voyons cela de plus près au travers de quelques exemple, et comment on parvient à résoudre ce problème avec plus ou moins de bonheur selon le langage de programmation choisi.


## Type standard et type métier

Prenons l'exemple du prix et quantité en e-commerce. Nous souhaitons créer un type Price autour d'un *double* et un type Quantity autour d'un *int* afin d'exprimer au mieux le métier avec des types. Cela amène aussi la protection du typage, pour éviter de passer un int à la place d'un autre par erreur. 

En Java, il faut donc créer une classe pour chaque:

     public class Price{
        private final double value;
        public Price(double value){this.value = value;}
     }

     public class Quantity{
        private final int value;
        public Quantity(int value){this.value = value;}
     }

Maintenant nous voulons multiplier le prix par la quantité pour calculer le prix total, qui doit être aussi de type Price:

    public class Price{
        ...
        Price multiply(int quantity){
          return new Price(this.value * quantity);
        }
     }


Nous aimerions pouvoir passer directement une instance de Quantity en paramètre, mais cela nous oblige ou bien à sortir la valeur primitive:

    myPrice.multiply(myQuantity.asDouble());

Ou bien à modifier la méthode multiply() pour accepter le type Quantity, ce qui la rend désormais spécifique à ce type, et donc couplée par la même occasion:

     Price multiply(Quantity quantity){
        return new Price(this.value * quantity.asDouble());
     }

En Haskell, la notion de synonym ou alias aide à avoir le beurre et l'argent du beurre dans une telle situation:


     type Quantity = Int

Et voilà! Nous avons un type sur-mesure, mais que nous pouvons aussi utiliser comme un entier quand nous le souhaitons, car il reste aussi un entier:

    multiply quantity = Price * quantity 
    // Cyrille -> Patrick??? Et la type-safety, on l'a vraiment toujours ? Si on attend une Quantity on peut passer un autre Int quelconque ?


D'autres façons de contourner le problème d'être à la fois un type standard et un type sur-mesure métier sont la conversion implicite (implicit cast en C++, C# ou Scala, et *deconstructor* en Haskell). Java supports a similar mechanism, called "unboxing", but only for the built-in primitive types.   


## Monoid standard et monoid métier

Maintenant nous souhaitons additionner des prix. Nous définissons donc une méthode add() à la class Price:

    Price add(Price other){
      return new Price(this.value + other.value);
    } 

Et par confort, j'ajoute le prix zero, bien utile dans de nombreux cas:

    public final static Price ZERO = new Price(0.);

Nous avons donc de fait un type qui obéit à la structure et aux propriétés d'un [Monoid](https://en.wikipedia.org/wiki/Monoid). Pas d'inquiétude si vous ne savez pas ce que c'est, ce n'est pas indispensable pour cet article ! 

Imaginons que nous avons une interface Monoid disponible que nous utilisons déjà un peu partout :

    public interface Monoid<M> {

      public M append(M m1, M m2);

      public M empty();

      default M reduceStream(Stream<M> ms) {
        return ms.reduce(empty(), this::append);
      }
      //... 
} 

Nous souhaitons alors que Price implémente l'interface Monoid:

    class Price implements Monoid<Price>{

    }

Cela nous oblige à définir les méthodes obligatoires **génériques** append() et empty(), qui renvoient vers les membres **expressifs** du métier, la méthode add() et le champs ZERO.

En F# ou Haskell, il est possible là aussi de définir des alias de fonctions. Cela permet d'avoir une fonction nommée selon le domaine métier, tout en étant en même temps l'implémentation d'une fonction définie dans une "interface" sous un autre nom:

    TODO Patrick


## Predicate standard et predicate métier

Un dernier exemple pour la route, cette fois avec les Predicate Java 8 et une astuce !

Nous avons de nombreuses règles de dégressivité sur les quantités, que nous avons modélisées sous forme de critères :

    public interface QuantityCriteria{
       boolean isSatisfied(Quantity q);
    } 

Le nommage correspond au langage du métier dans notre domaine. Cependant, il est évident que notre critère n'est autre qu'un prédicat, et pour pouvoir utiliser toute la plomberie fournie par le langage de programmation autour des prédicats, il faudrait implémenter l'interface Predicate standard:

    public interface QuantityCriteria extends Predicate<Quantity>{
       boolean isSatisfied(Quantity q);
    } 

Ce qui oblige alors à définir en plus la méthode obligatoire test():

    public interface QuantityCriteria extends Predicate<Quantity>{
       boolean isSatisfied(Quantity q);
       boolean test(Quantity q);
    } 

Il faut de plus que la méthode test() renvoie à la méthode isSatisfied(). Pour éviter de faire celà dans chaque implémentation concrète, nous pouvons utiliser une méthode *default* dans notre interface:

    public interface QuantityCriteria extends Predicate<Quantity>{
       boolean isSatisfied(Quantity q);
       default boolean test(Quantity q){return this::isSatisfied(q);}
    }

Et voilà ! Nous pouvons donc désormais passer n'importe quelle instance de QuantityCriteria à toute fonction qui attend un Predicate en paramètre, c'est cool. 

Notez au passage que QuantityCriteria n'a qu'une seule méthode abstraite, et est donc une FunctionalInterface, il est donc possible de passer une instance de QuantityCriteria partout où une lambda de même signature est attendue :

    myStream.filter(myQuantityCritera);

Imaginons que nous voulons composer plusieurs QuantityCriteria avec un AND logique, en utilisant l

    QuantityCriteria over10 = new QuantityOver(10);
    QuantityCriteria below100 = new QuantityBelow(100);
    Predicate<Quantity> between10And100 = over10.and(below100);

Notice that the built-in and() method returned a plain Predicate, not a QuantityCriteria. Sniff ! Pourtant nous souhaitons utiliser les critères composés dans une méthode qui attend un QuantityCriteria :

    class PurchaseHistory...
       boolean isEligible(QuantityCriteria criteria);

Peut-on alors écrire cela ?

    boolean eligible = PurchaseHistory.isEligible(between10And100);

Par la magie des functional interfaces de Java 8 ?

// Patrick ?