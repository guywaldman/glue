# /// script
# dependencies = [
#   "pydantic",
#   "requests",
# ]
# ///

import datetime
import hashlib
import hmac
import sys
import time
import uuid
import requests
import xml.etree.ElementTree as ET
from urllib.parse import quote

if len(sys.argv) != 4:
    raise SystemExit(
        "usage: s3_minio_http.py <generated_module_parent_dir> <generated_module_name> <port>"
    )

# Dynamically resolve the module where the Glue-generated models are defined.
module_parent = sys.argv[1]
module_name = sys.argv[2]
port = sys.argv[3]

sys.path.insert(0, module_parent)
mod = __import__(module_name)


ACCESS_KEY = "minioadmin"
SECRET_KEY = "minioadmin"
REGION = "us-east-1"
SERVICE = "s3"
ENDPOINT = f"http://127.0.0.1:{port}"
HOST = f"127.0.0.1:{port}"


def _hmac(key: bytes, msg: str) -> bytes:
    return hmac.new(key, msg.encode("utf-8"), hashlib.sha256).digest()


def _signing_key(secret_key: str, date_stamp: str, region: str, service: str) -> bytes:
    k_date = _hmac(("AWS4" + secret_key).encode("utf-8"), date_stamp)
    k_region = _hmac(k_date, region)
    k_service = _hmac(k_region, service)
    return _hmac(k_service, "aws4_request")


def _encode_uri(path: str) -> str:
    return quote(path, safe="/-_.~")


def _encode_qs(value: str) -> str:
    return quote(value, safe="-_.~")


def _canonical_query(params: dict[str, str] | None) -> str:
    if not params:
        return ""
    pairs = [(_encode_qs(str(k)), _encode_qs(str(v))) for k, v in params.items()]
    pairs.sort()
    return "&".join(f"{k}={v}" for k, v in pairs)


def _build_auth_headers(
    method: str,
    canonical_uri: str,
    canonical_query: str,
    body: bytes,
    extra_headers: dict[str, str] | None,
):
    now = datetime.datetime.now(datetime.timezone.utc)
    amz_date = now.strftime("%Y%m%dT%H%M%SZ")
    date_stamp = now.strftime("%Y%m%d")
    payload_hash = hashlib.sha256(body).hexdigest()

    headers = {
        "host": HOST,
        "x-amz-date": amz_date,
        "x-amz-content-sha256": payload_hash,
    }
    if extra_headers:
        for k, v in extra_headers.items():
            headers[k.lower()] = str(v)

    sorted_items = sorted((k, " ".join(v.strip().split())) for k, v in headers.items())
    canonical_headers = "".join(f"{k}:{v}\n" for k, v in sorted_items)
    signed_headers = ";".join(k for k, _ in sorted_items)

    canonical_request = "\n".join(
        [
            method,
            canonical_uri,
            canonical_query,
            canonical_headers,
            signed_headers,
            payload_hash,
        ]
    )

    credential_scope = f"{date_stamp}/{REGION}/{SERVICE}/aws4_request"
    string_to_sign = "\n".join(
        [
            "AWS4-HMAC-SHA256",
            amz_date,
            credential_scope,
            hashlib.sha256(canonical_request.encode("utf-8")).hexdigest(),
        ]
    )

    signing_key = _signing_key(SECRET_KEY, date_stamp, REGION, SERVICE)
    signature = hmac.new(
        signing_key, string_to_sign.encode("utf-8"), hashlib.sha256
    ).hexdigest()

    authorization = (
        f"AWS4-HMAC-SHA256 Credential={ACCESS_KEY}/{credential_scope}, "
        f"SignedHeaders={signed_headers}, Signature={signature}"
    )

    headers["authorization"] = authorization
    return headers


def signed_request(
    method: str,
    path: str,
    *,
    params: dict[str, str] | None = None,
    headers: dict[str, str] | None = None,
    body: bytes | str | None = None,
):
    data = (
        b""
        if body is None
        else (body.encode("utf-8") if isinstance(body, str) else body)
    )
    canonical_uri = _encode_uri(path)
    canonical_query = _canonical_query(params)

    signed_headers = _build_auth_headers(
        method, canonical_uri, canonical_query, data, headers
    )

    request_headers = {k: v for k, v in signed_headers.items() if k != "host"}
    url = f"{ENDPOINT}{canonical_uri}"
    if canonical_query:
        url = f"{url}?{canonical_query}"

    return requests.request(method, url, headers=request_headers, data=data, timeout=10)


def object_path(bucket: str, key: str) -> str:
    return f"/{bucket}/{quote(key, safe='/-_.~')}"


last_error = None
for _ in range(30):
    try:
        health = requests.get(f"{ENDPOINT}/minio/health/ready", timeout=2)
        if health.status_code == 200:
            break
        last_error = f"status={health.status_code}"
    except Exception as exc:
        last_error = exc
    time.sleep(0.5)
else:
    raise RuntimeError(f"MinIO did not become ready: {last_error}")

bucket = mod.BucketRef(name=f"glue-e2e-{uuid.uuid4().hex[:8]}")
create_bucket_resp = signed_request("PUT", f"/{bucket.name}")
assert create_bucket_resp.status_code in (200, 204), create_bucket_resp.text
create_bucket = mod.CreateBucketResult(
    bucket=bucket.name, status_code=create_bucket_resp.status_code
)
assert create_bucket.bucket == bucket.name

req1 = mod.PutObjectRequest(
    bucket=bucket.name,
    key="nested/hello.txt",
    body="hello from glue",
    content_type="text/plain",
    metadata={"owner": "glue", "env": "e2e"},
    tags=[
        mod.ObjectTag(key="team", value="platform"),
        mod.ObjectTag(key="type", value="text"),
    ],
)

req2 = mod.PutObjectRequest(
    bucket=bucket.name,
    key="nested/data.json",
    body='{"x":1,"y":2}',
    content_type="application/json",
    metadata={"owner": "glue", "format": "json"},
    tags=[
        mod.ObjectTag(key="team", value="platform"),
        mod.ObjectTag(key="type", value="json"),
    ],
)


def tag_query(tags):
    if not tags:
        return None
    return "&".join(f"{_encode_qs(t.key)}={_encode_qs(t.value)}" for t in tags)


def put_and_model(req):
    request_headers = {"content-type": req.content_type or "application/octet-stream"}
    for k, v in (req.metadata or {}).items():
        request_headers[f"x-amz-meta-{k.lower()}"] = v
    tq = tag_query(req.tags)
    if tq:
        request_headers["x-amz-tagging"] = tq

    resp = signed_request(
        "PUT", object_path(req.bucket, req.key), headers=request_headers, body=req.body
    )
    assert resp.status_code in (200, 204), resp.text
    return mod.PutObjectResult(
        key=req.key, e_tag=resp.headers.get("ETag", "").strip('"')
    )


put1 = put_and_model(req1)
put2 = put_and_model(req2)

assert put1.key == req1.key
assert put2.key == req2.key
assert len(put1.e_tag) > 0
assert len(put2.e_tag) > 0

list_resp = signed_request(
    "GET", f"/{bucket.name}", params={"list-type": "2", "prefix": "nested/"}
)
assert list_resp.status_code == 200, list_resp.text
root = ET.fromstring(list_resp.text)
ns = {"s3": "http://s3.amazonaws.com/doc/2006-03-01/"}
listed = []
for content in root.findall("s3:Contents", ns):
    listed.append(
        mod.ListedObject(
            key=content.findtext("s3:Key", default="", namespaces=ns),
            size=int(content.findtext("s3:Size", default="0", namespaces=ns)),
            e_tag=(content.findtext("s3:ETag", default="", namespaces=ns) or "").strip(
                '"'
            ),
        )
    )
listed_keys = sorted(o.key for o in listed)
assert listed_keys == ["nested/data.json", "nested/hello.txt"]

head1_raw = signed_request("HEAD", object_path(bucket.name, req1.key))
assert head1_raw.status_code == 200, head1_raw.text
head1 = mod.HeadObjectResult(
    key=req1.key,
    content_length=int(head1_raw.headers.get("Content-Length", "0")),
    content_type=head1_raw.headers.get("Content-Type"),
    metadata={
        k[len("x-amz-meta-") :].lower(): v
        for k, v in head1_raw.headers.items()
        if k.lower().startswith("x-amz-meta-")
    },
)
assert head1.content_type == req1.content_type
assert head1.metadata.get("owner") == "glue"

copy_req = mod.CopyObjectRequest(
    src_bucket=bucket.name,
    src_key=req1.key,
    dst_bucket=bucket.name,
    dst_key="nested/copied-hello.txt",
)
copy_source = f"/{copy_req.src_bucket}/{quote(copy_req.src_key, safe='/-_.~')}"
copy_resp = signed_request(
    "PUT",
    object_path(copy_req.dst_bucket, copy_req.dst_key),
    headers={"x-amz-copy-source": copy_source},
)
assert copy_resp.status_code in (200, 204), copy_resp.text

resp = signed_request(
    "GET",
    object_path(copy_req.dst_bucket, copy_req.dst_key),
    headers={"accept": "text/plain"},
)
assert resp.status_code == 200, resp.text
result = mod.GetObjectResult(
    key=copy_req.dst_key, body=resp.text, content_length=len(resp.content)
)

assert result.key == copy_req.dst_key
assert result.body == req1.body
assert result.content_length == len(req1.body.encode("utf-8"))

delete_req = mod.DeleteObjectsRequest(
    bucket=bucket.name, keys=[req1.key, req2.key, copy_req.dst_key]
)
for key in delete_req.keys:
    delete_resp = signed_request("DELETE", object_path(delete_req.bucket, key))
    assert delete_resp.status_code in (200, 204), delete_resp.text
    deleted = mod.DeleteObjectResult(key=key, status_code=delete_resp.status_code)
    assert deleted.key == key

listed_after_resp = signed_request(
    "GET", f"/{bucket.name}", params={"list-type": "2", "prefix": "nested/"}
)
assert listed_after_resp.status_code == 200, listed_after_resp.text
root_after = ET.fromstring(listed_after_resp.text)
key_count_text = root_after.findtext("s3:KeyCount", default="0", namespaces=ns)
assert int(key_count_text) == 0

print("MinIO + Glue generated models over signed HTTP flow OK")
