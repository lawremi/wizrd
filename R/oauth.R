### Barebones client implementation of RFCs 7591, 8414, 8707 and 9728

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
        resource = nullable(scalar(class_character)),
        redirect_uri = scalar(class_character),
        auth_url = scalar(class_character),
        cache_disk = scalar(class_logical, default = FALSE)
    )
)

req_oauth_with := new_generic(c("req", "auth"))

method(req_oauth_with, list(S3_httr2_request, OAuthAuthCode)) <-
    function(req, auth) {
        params <- as.list(c(resource = auth@resource))
        httr2::req_oauth_auth_code(req, auth@client, auth@auth_url, auth@scope,
                                   auth_params = params, token_params = params,
                                   redirect_uri = auth@redirect_uri,
                                   cache_disk = auth@cache_disk)
    }

oauth_server_metadata <- function(url) {
    httr2::request(url) |>
        httr2::req_url_path(".well-known/oauth-authorization-server") |>
        httr2::req_url_path_append(httr2::url_parse(url)$path) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
}

oauth_register_client <- function(url, redirect_uri,
                                  package_name = getNamespaceName(topenv())) {
    httr2::request(url) |>
        httr2::req_body_json(dcr_body(package_name, redirect_uri)) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
}

redirect_uri_with_port <- function() {
    uri <- httr2::oauth_redirect_uri()
    parsed <- httr2::url_parse(uri)
    parsed$port <- parsed$port %||% find_available_port()
    httr2::url_build(parsed)
}

dcr_body <- function(package_name, redirect_uri) {
    list(redirect_uris = list(redirect_uri),
         token_endpoint_auth_method = "client_secret_post",
         grant_types = list("authorization_code"),
         client_name = package_name,
         client_uri = packageDescription(package_name)$URL)
}

oauth_client_from_dcr <- function(dcr, token_endpoint) {
    httr2::oauth_client(dcr$client_id, token_endpoint, dcr$client_secret,
                        name = dcr$client_name)
}

check_oauth_server_metadata <- function(meta) {
    if (is.null(meta$authorization_endpoint))
        stop("no authorization endpoint in server metadata")
    if (is.null(meta$token_endpoint))
        stop("no token endpoint in server metadata")
    if (!is.null(meta$token_endpoint_auth_methods_supported) &&
            !any(c("none", "client_secret_post") %in%
                     meta$token_endpoint_auth_methods_supported))
        stop("server does not support 'client_secret_post' authorization")
    if (!is.null(meta$grant_types_supported) &&
            !"authorization_code" %in% meta$grant_types_supported)
        stop("server does not support 'authorization_code' grants")
}

oauth_client_for_server_metadata <- function(meta, redirect_uri) {
    if (!is.null(meta$registration_endpoint))
        oauth_register_client(meta$registration_endpoint, redirect_uri) |>
            oauth_client_from_dcr(meta$token_endpoint)
    else httr2::oauth_client(getNamespaceName(topenv()), meta$token_endpoint)
}

oauth_auth_code_for_server_metadata <- function(meta, ...) {
    check_oauth_server_metadata(meta)
    redirect_uri <- redirect_uri_with_port()
    client <- oauth_client_for_server_metadata(meta, redirect_uri)
    OAuthAuthCode(client = client, redirect_uri = redirect_uri,
                  auth_url = meta$authorization_endpoint, ...)
}

oauth_auth_code_for_server <- function(url, ...) {
    oauth_server_metadata(url) |>
        oauth_auth_code_for_server_metadata(...)
}

check_oauth_resource_metadata <- function(meta) {
    if (!"header" %in% (meta$bearer_methods_supported %||% "header"))
        "resource does not support 'header' bearer method"
}

oauth_auth_code_for_resource_metadata <- function(meta, ...) {
    check_oauth_resource_metadata(meta)
    if (length(meta$authorization_servers) == 0L)
        stop("No authorization servers found in resource metadata")
    ### FIXME: how to enable user to choose when there are multiple servers?
    ### Perhaps a per-resource option, falling back to choose()?
    meta$authorization_servers[[1L]] |>
        oauth_auth_code_for_server(resource = meta$resource, ...)
}

oauth_resource_metadata <- function(url) {
    httr2::request(url) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
}

oauth_auth_code_for_resource_url <- function(url, ...) {
    oauth_resource_metadata(url) |>
        oauth_auth_code_for_resource_metadata(...)
}

oauth_resource_url_from_response <- function(resp) {
    resp |> httr2::resp_header("WWW-Authenticate") |>
        sub(".*resource_metadata *= *\"([^\"]*).*", "\\1", x = _)
}

oauth_auth_code_for_response <- function(resp, ...) {
    resp |> oauth_resource_url_from_response() |>
        oauth_auth_code_for_resource_url(...)
}
