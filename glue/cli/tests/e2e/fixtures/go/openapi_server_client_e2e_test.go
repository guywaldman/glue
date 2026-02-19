package apitest

import (
	"context"
	"encoding/json"
	"net/http"
	"net/http/httptest"
	"testing"
)

type stubServer struct{}

func strPtr(v string) *string {
	return &v
}

func writeJSON(w http.ResponseWriter, status int, body any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	_ = json.NewEncoder(w).Encode(body)
}

func (s *stubServer) GetUser(w http.ResponseWriter, r *http.Request, userId string) {
	if userId == "boom" {
		writeJSON(w, http.StatusInternalServerError, ApiError{
			Code:    500,
			Message: "backend exploded",
		})
		return
	}

	if userId == "missing" {
		writeJSON(w, http.StatusNotFound, ApiError{
			Code:    404,
			Message: "user not found",
			Details: &map[string]string{"user_id": userId},
		})
		return
	}

	writeJSON(w, http.StatusOK, UserResponse{
		RequestId: strPtr("req-1"),
		User: User{
			UserId:      strPtr(userId),
			Name:        "Ada Lovelace",
			Age:         32,
			Active:      true,
			Role:        "admin",
			Tags:        []string{"core", "ops"},
			Preferences: map[string]string{"theme": "dark", "lang": "en"},
			Address: &Address{
				Street:  "1 Analytical Engine Rd",
				City:    "London",
				Country: "UK",
			},
		},
	})
}

func (s *stubServer) ListUsers(w http.ResponseWriter, r *http.Request) {
	users := []User{
		{UserId: strPtr("user_123"), Name: "Ada Lovelace", Age: 32, Active: true, Role: "admin", Tags: []string{"core"}, Preferences: map[string]string{"theme": "dark"}},
		{UserId: strPtr("user_456"), Name: "Grace Hopper", Age: 37, Active: true, Role: "viewer", Tags: []string{"compiler"}, Preferences: map[string]string{"theme": "light"}},
	}

	writeJSON(w, http.StatusOK, UserListResponse{Users: users, Total: len(users)})
}

func (s *stubServer) ListUserPosts(w http.ResponseWriter, r *http.Request, userId string) {
	if userId == "missing" {
		writeJSON(w, http.StatusNotFound, ApiError{Code: 404, Message: "user not found"})
		return
	}

	posts := []Post{
		{PostId: strPtr("p_001"), Title: "Intro", Published: true},
		{PostId: strPtr("p_002"), Title: "Advanced", Published: false},
	}

	writeJSON(w, http.StatusOK, PostListResponse{Posts: posts, Total: len(posts)})
}

func (s *stubServer) HealthCheck(w http.ResponseWriter, r *http.Request) {
	writeJSON(w, http.StatusOK, HealthResponse{Status: "ok", Version: "1.0.0"})
}

func TestGeneratedOpenAPIServerClientRoundtrip(t *testing.T) {
	h := Handler(&stubServer{})
	ts := httptest.NewServer(h)
	defer ts.Close()

	client, err := NewClientWithResponses(ts.URL)
	if err != nil {
		t.Fatalf("new client: %v", err)
	}

	ctx := context.Background()

	getUserResp, err := client.GetUserWithResponse(ctx, "user_123")
	if err != nil {
		t.Fatalf("get user request: %v", err)
	}
	if getUserResp.StatusCode() != 200 || getUserResp.JSON200 == nil {
		t.Fatalf("unexpected get user response: status=%d body=%s", getUserResp.StatusCode(), string(getUserResp.Body))
	}
	if getUserResp.JSON200.User.UserId == nil || *getUserResp.JSON200.User.UserId != "user_123" {
		t.Fatalf("unexpected user id: %v", getUserResp.JSON200.User.UserId)
	}

	missingUserResp, err := client.GetUserWithResponse(ctx, "missing")
	if err != nil {
		t.Fatalf("missing user request: %v", err)
	}
	if missingUserResp.StatusCode() != 404 {
		t.Fatalf("unexpected missing user response: status=%d body=%s", missingUserResp.StatusCode(), string(missingUserResp.Body))
	}
	var missingErr ApiError
	if err := json.Unmarshal(missingUserResp.Body, &missingErr); err != nil {
		t.Fatalf("failed to decode missing user error: %v", err)
	}
	if missingErr.Code != 404 {
		t.Fatalf("unexpected missing error code: %d", missingErr.Code)
	}

	serverErrResp, err := client.GetUserWithResponse(ctx, "boom")
	if err != nil {
		t.Fatalf("server error request: %v", err)
	}
	if serverErrResp.StatusCode() != 500 {
		t.Fatalf("unexpected server error response: status=%d body=%s", serverErrResp.StatusCode(), string(serverErrResp.Body))
	}
	var serverErr ApiError
	if err := json.Unmarshal(serverErrResp.Body, &serverErr); err != nil {
		t.Fatalf("failed to decode server error: %v", err)
	}
	if serverErr.Code != 500 {
		t.Fatalf("unexpected server error code: %d", serverErr.Code)
	}

	listUsersResp, err := client.ListUsersWithResponse(ctx)
	if err != nil {
		t.Fatalf("list users request: %v", err)
	}
	if listUsersResp.StatusCode() != 200 || listUsersResp.JSON200 == nil {
		t.Fatalf("unexpected list users response: status=%d body=%s", listUsersResp.StatusCode(), string(listUsersResp.Body))
	}
	if listUsersResp.JSON200.Total != 2 {
		t.Fatalf("unexpected list user total: %d", listUsersResp.JSON200.Total)
	}

	listPostsResp, err := client.ListUserPostsWithResponse(ctx, "user_123")
	if err != nil {
		t.Fatalf("list posts request: %v", err)
	}
	if listPostsResp.StatusCode() != 200 || listPostsResp.JSON200 == nil {
		t.Fatalf("unexpected list posts response: status=%d body=%s", listPostsResp.StatusCode(), string(listPostsResp.Body))
	}
	if listPostsResp.JSON200.Total != 2 {
		t.Fatalf("unexpected posts total: %d", listPostsResp.JSON200.Total)
	}

	healthResp, err := client.HealthCheckWithResponse(ctx)
	if err != nil {
		t.Fatalf("health request: %v", err)
	}
	if healthResp.StatusCode() != 200 || healthResp.JSON200 == nil {
		t.Fatalf("unexpected health response: status=%d body=%s", healthResp.StatusCode(), string(healthResp.Body))
	}
	if healthResp.JSON200.Status != "ok" {
		t.Fatalf("unexpected health status: %s", healthResp.JSON200.Status)
	}
}
