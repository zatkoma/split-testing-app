# SEO Split Testing Evaluation
Změna titulků, obsahu, interního prolinkování, příprava textů a další úpravy mohou mít pozitivní nebo negativní dopad na váš web. Možná jste si říkali, jak tyto úpravy jednoduše doměřit a vyhodnotit. Aplikace **SEO Split Testing Evaluation** vám s vyhodnocováním úspěšnosti pomůže.

## Jak aplikace funguje?

## Jak aplikaci nainstalovat?

### Co potřebujete?

- R (nejnovější verze)
- Přístup ke GSC vašich projektů

### Informace pro R neznalé:

- Stáhněte si R
- Stáhněte si RStudio
- Stáhněte si tento repozitář (v .zip)
- Otevřete jej do libovolné složky
- Otevřete projekt
- Spusťe instalační příkaz na balíčky (nalezne v setup.r)
- Spusťe aplikaci

### Informace pro R znalé:
Předpokládám, že všechno budete mít již stažené, tudíž to rovnou můžete spustit. :)

## Možné problémy, které vás potkají:

V aplikaci není napsaná bezpečnostní vrstva, která by kontrolovala, že vámi zadané přístupy jsou správné a jakmile se něco nástroji nebude líbit, tak krutě spadne a v lepším případě vám řekne nějakou chytrou hlášku. Jestliže bude o tento nástroj zájem, tak není problém jej rozvíjet i v tomto směru. :)

**Co dělat, když to spadne?**

Jestliže aplikace spadla, tak ji zkuste spustit znovu a při opětovném spuštění dávat pozor na tyto možné problémy.

- [ ] Máte přístupová práva k webu (ověřeno)?
- [ ] Existují vámi srovnávané URL adresy?
- [ ] Je vybraná dostatečná délka období?
- [ ] Je období před změnou minimálně trojnásobné jak období po změně?
- [ ] Neprovedli jste náhodou změnu ve struktuře URL adres?
- [ ] Zaškrtli jste, že srovnáváte adresáře, když chcete srovnat adresáře?

## Credits

Inspiraci (problematiku split testů) nadhodil David Brener. Martin Žatkovič přišel s implementací aplikace.