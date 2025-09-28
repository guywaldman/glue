/** A user in the system. */
interface User {
  /** The user's unique identifier */
  id: number;
  /** The user's name */
  name: string;
  /** The user's email address */
  email: string;
  /** Whether the user is active */
  is_active: boolean;
}

/**
 * A post made by a user.
 * It can contain text and an optional image
 */
interface Post {
  /** The post's unique identifier */
  id: User;
  /** Either the ID of the user who made the post, or its data */
  user: number | User;
  /** The content of the post */
  content: string;
  /** An optional URL to an image associated with the post */
  image_url: string;
}
