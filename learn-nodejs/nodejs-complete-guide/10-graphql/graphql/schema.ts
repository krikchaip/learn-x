import { buildSchema } from 'graphql'

export default buildSchema(`#graphql
  schema {
    query: Query
    mutation: Mutation
  }

  type Query {
    user: UserQuery!
    post: PostQuery!
  }

  type Mutation {
    user: UserMutation!
    post: PostMutation!
  }

  type UserQuery {
    list(token: String!): [User!]!
    login(email: String!, password: String!): UserLogin!
  }

  type UserMutation {
    create(email: String!, password: String!, role: Role = USER): User!
    update(token: String!, data: UserUpdate!): User!
  }

  input UserUpdate {
    name: String
    status: String
  }

  type PostQuery {
    list: [Post!]!
  }

  type PostMutation {
    create(data: PostCreate!): Post!
  }

  input PostCreate {
    creator: ID!
    title: String!
    content: String
    imageURL: String
  }

  enum Role {
    USER
    ADMIN
  }

  type User {
    id: ID!
    email: String!
    password: String!
    role: Role!
    name: String
    status: String
    posts: [Post!]!
  }

  type UserLogin {
    token: String!
    data: User!
  }

  type Post {
    id: ID!
    creator: User!
    title: String!
    content: String
    imageURL: String
  }
`)
