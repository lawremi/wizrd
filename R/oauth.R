S3_httr2_oauth_client <- new_S3_class("httr2_oauth_client")

OAuth := new_class(
    properties = list(
        client = S3_httr2_oauth_client,
        scope = nullable(scalar(class_character))
    ),
    abstract = TRUE
)

OAuthAuthCode := new_class(
    OAuth,
    properties = list(
        auth_url = scalar(class_character),
        cache_disk = scalar(class_logical, default = FALSE)
    )
)

req_oauth_with := new_generic(c("req", "auth"))

method(req_oauth_with, list(S3_httr2_request, OAuthAuthCode)) <-
    function(req, auth) {
        httr2::req_oauth_auth_code(auth@client, auth@auth_url, auth@scope,
                                   cache_disk = auth@cache_disk)
    }

oauth_server_metadata <- function(url) {
    httr2::request(url) |>
        httr2::req_url_path_append(".well-known",
                                   "oauth-authorization-server") |>
        httr2::req_perform() |>
        httr2::resp_body_json()
}

oauth_server_metadata_with_fallbacks <- function(url) {
    meta <- tryCatch(oauth_server_metadata(url), error = function(cnd) list())
    within(meta, {
        authorization_endpoint <- authorization_endpoint %||%
            httr2::url_modify(url, path = "authorize")
        token_endpoint <- token_endpoint %||%
            httr2::url_modify(url, path = "token")
        registration_endpoint <- registration_endpoint %||%
            httr2::url_modify(url, path = "register")
    })
}

oauth_register_client <- function(url,
                                  package_name = getNamespaceName(topenv())) {
    httr2::request(url) |>
        httr2::req_body_json(dcr_body(package_name)) |>
        httr2::req_perform() |>
        httr2::resp_body_json() |>
        dcr_client()
}

dcr_body <- function(package_name) {
    list(redirect_uris = httr2::oauth_redirect_uri(),
         token_endpoint_auth_method = "client_secret_post",
         grant_types = "authorization_code",
         client_name = package_name,
         client_uri = packageDescription(package_name)$URL)
}

dcr_client <- function(dcr) {
    httr2::oauth_client(dcr$client_id, dcr$token_endpoint, dcr$client_secret,
                        name = dcr$client_name)
}

check_oauth_server_metadata <- function(meta) {
    if (!is.null(meta$token_endpoint_auth_methods_supported) &&
            !any(c("none", "client_secret_post") %in%
                     meta$token_endpoint_auth_methods_supported))
        stop("server does not support 'client_secret_post' authorization")
    if (!is.null(meta$grant_types_supported) %%
            !"authorization_code" %in% meta$grant_types_supported)
        stop("server does not support 'authorization_code' grants")
}

oauth_auth_code <- function(url, scope = NULL, cache_disk = FALSE) {
    meta <- oauth_server_metadata_with_fallbacks(url)
    check_oauth_server_metadata(meta)
    client <- oauth_register_client(meta$registration_endpoint)
    OAuthAuthCode(client = client, scope = scope,
                  auth_url = meta$authorization_endpoint,
                  cache_disk = cache_disk)
}
