
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- AuthHandler defineix el formulari de login, les funcions de controlador relacionades amb l'autenticació i les vistes corresponents. Les funcions de controlador gestionen l'autenticació de l'usuari, validen les credencials i inicien o tanquen la sessió de l'usuari. Les vistes mostren el formulari de login i altres elements relacionats amb l'autenticació.

module AuthHandler
where
import           Found
import           Model
import           Db

import           Develop.DatFw
import           Develop.DatFw.Widget
import           Develop.DatFw.Template
import           Develop.DatFw.Form
import           Develop.DatFw.Form.Fields

import           Data.Text (Text)
import           Control.Monad            -- imports when, ...
import           Control.Monad.Trans.Maybe
import           Text.Blaze


-- ****************************************************************
-- Sistema d'autenticacio.

-- Handlers del sitema d'autenticació.

loginForm :: MonadHandler m => AForm m (Text, Text)
loginForm =
    (,) <$> freq textField "Nom d'usuari" Nothing
        <*> freq passwordField "Clau d'accés" Nothing

-- loginForm :: MonadHandler m => AForm m (Text, Text): Aquesta és una definició d'un formulari de login utilitzant el tipus AForm del framework DatFw. El formulari té dos camps: "Nom d'usuari" i "Clau d'accés". Utilitza els combinadors freq per especificar els camps del formulari i els tipus de camps (textField i passwordField). Retorna una tupla amb els valors introduïts en els camps del formulari.

getLoginR :: HandlerFor ForumsApp Html
getLoginR = do
    setUltDestReferer
    -- Return HTML page
    (_, formw) <- runAFormPost loginForm
    defaultLayout $ loginView formw

-- getLoginR :: HandlerFor ForumsApp Html: Aquesta és la funció de controlador per a la ruta de login amb el mètode GET. Estableix la referència de l'última destinació (setUltDestReferer) i després executa el formulari de login utilitzant la funció runAFormPost. A continuació, mostra la vista de login utilitzant la funció defaultLayout i passant el widget generat.


postLoginR :: HandlerFor ForumsApp Html
postLoginR = do
    toMaster <- getRouteToMaster
    (formr, formw) <- runAFormPost loginForm
    case formr of
        FormSuccess (name, password) -> do
            ok <- validatePassword name password
            if ok then do
                -- Good credentials
                Just uid <- runDbAction $ loginUser name
                setSession authId_SESSION_KEY $ toPathPiece uid
                redirectUltDest HomeR
            else do
                -- Login error
                setMessage "Error d'autenticaciò"
                redirect LoginR
        _ ->
            defaultLayout (loginView formw)

--postLoginR :: HandlerFor ForumsApp Html: Aquesta és la funció de controlador per a la ruta de login amb el mètode POST. Obté la funció getRouteToMaster per generar les rutes de redirecció i executa el formulari de login utilitzant runAFormPost. A continuació, processa el resultat del formulari. Si el formulari és vàlid i les credencials d'autenticació són correctes, inicia la sessió de l'usuari amb setSession i redirigeix a la pàgina d'inici (HomeR). En cas contrari, mostra un missatge d'error i redirigeix a la pàgina de login (LoginR).  
    
    where
        validatePassword :: Text -> Text -> HandlerFor ForumsApp Bool
        validatePassword name password = do
            mbuser <- runDbAction $ getUserByName name
            case mbuser of
                Nothing -> pure False
                Just (_, user) -> pure $ pHashValidate password $ udPassword user

--validatePassword :: Text -> Text -> HandlerFor ForumsApp Bool: Aquesta funció valida la contrasenya de l'usuari comparant-la amb la contrasenya emmagatzemada a la base de dades. Utilitza la funció getUserByName per obtenir les dades de l'usuari i compara la contrasenya amb la funció pHashValidate del framework DatFw.

handleLogoutR :: HandlerFor ForumsApp ()
handleLogoutR = do
    -- | After logout (from the browser), redirect to the referring page.
    setUltDestReferer
    deleteSession authId_SESSION_KEY
    redirectUltDest HomeR

--handleLogoutR :: HandlerFor ForumsApp (): Aquesta funció de controlador s'encarrega de gestionar la desconnexió de l'usuari. Estableix la referència de l'última destinació i elimina la sessió de l'autenticació (deleteSession). Finalment, redirigeix a la pàgina d'inici (HomeR).



getChangePasswordR :: HandlerFor ForumsApp Html
getChangePasswordR = do
    user <- requireAuth
    (_, formw) <- runAFormPost changePasswordForm --executa la funció runAFormPost amb el formulari changePasswordForm. Aquesta funció processa les dades enviades pel formulari de canvi de contrasenya i retorna un widget amb els resultats (guardat a la variable formw)
    defaultLayout $ changePasswordView formw -- changePasswordView és una vista específica per aquesta pàgina i rep el widget formw com a argument per mostrar el formulari de canvi de contrasenya.

postChangePasswordR :: HandlerFor ForumsApp Html
postChangePasswordR = do
    user <- requireAuth
    (formr, formw) <- runAFormPost changePasswordForm -- funció processa les dades enviades pel formulari de canvi de contrasenya i retorna una tupla amb dos valors: formr que representa el resultat del processament del formulari i formw que és el widget de la vista del formulari.
    case formr of --avaluació del resultat del formulari
        FormSuccess (oldPassword, newPassword, confirmPassword) -> -- si s'ha enviat correctament
            if newPassword == confirmPassword then do
                passwordChanged <- runDbAction $ changeUserPassword user oldPassword newPassword
                if passwordChanged then do
                    setMessage "La contrassenya s'ha canviat."
                    redirect HomeR
                else do
                    setMessage "Contrassenya antiga incorrecta."
                    redirect ChangePasswordR
            else do
                setMessage "La nova contrassenya no coincideix amb l'antiga."
                redirect ChangePasswordR
        _ ->
            defaultLayout $ changePasswordView formw

changePasswordForm :: MonadHandler m => AForm m (Text, Text, Text)
changePasswordForm =
    (,,) <$> freq passwordField "Contrassenya actual" Nothing
         <*> freq passwordField "Nova contrassenya" Nothing
         <*> freq passwordField "Confirma la nova contrassenya" Nothing

-- Falta fer el View i Html
-- ---------------------------------------------------------------
-- Vistes del sistema d'autenticacio.

loginView :: Widget ForumsApp -> Widget ForumsApp
loginView formw = [widgetTempl|
<div class="row">
  <div class="col-sm-offset-2 col-sm-10">
    <form role="form" method="POST" action="@{LoginR}">
      ^{formw}
      <div class="form-group"><button type="submit" class="btn btn-success">Entra</button></div>
    </form>
  </div>
</div>
|]

-- loginView :: Widget ForumsApp -> Widget ForumsApp: Aquesta funció defineix la vista de login utilitzant la sintaxi de plantilles del framework DatFw. Crea un formulari HTML amb els camps del formulari de login i un botó d'enviament. La vista s'incrusta dins d'un widget utilitzant la sintaxi [widgetTempl| ... |].


changePasswordView :: Widget ForumsApp -> Widget ForumsApp
changePasswordView formw = [widgetTempl|
<div class="row">
  <div class="col-sm-offset-2 col-sm-10">
    <form role="form" method="POST" action="@{ChangePasswordR}">
      ^{formw}
      <div class="form-group"><button type="submit" class="btn btn-success">Canvia el password</button></div>
    </form>
  </div>
</div>
|]
