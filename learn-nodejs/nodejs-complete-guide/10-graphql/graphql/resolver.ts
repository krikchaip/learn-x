import { Request } from 'express'
import validator from 'validator'

import Token from '@lib/token'
import User, { matchPassword, Role } from '@model/user'
import Post from '@model/post'

export default {
  user: {
    list: async (args: { token: string }) => {
      if (!Token.verify(args.token, { role: Role.ADMIN }))
        throw new Error('Unauthorized!')

      const users = await User.findAll({ include: 'posts' })
      return users.map(user => user.toJSON())
    },
    create: async (
      args: { email: string; password: string; role: Role },
      req: Request
    ) => {
      const errors = [
        !validator.isEmail(args.email) && 'Wrong email format!',
        validator.isEmpty(args.password) && 'No password entered!',
        !validator.isLength(args.password, { min: 5 }) &&
          'Password must contain atleast 5 characters!',
        (await User.findOne({ where: { email: args.email } })) &&
          'Email exists!'
      ]
        .filter((msg): msg is string => !!msg)
        .map(msg => new Error(msg))

      if (errors.length) {
        throw errors[0]
      }

      try {
        const user = (await User.create(args)).toJSON()
        return { ...user, posts: [] }
      } catch (error) {
        throw new Error(error.message || 'Internal server error!')
      }
    },
    update: async (args: {
      token: string
      data: Partial<{ name: string; status: string }>
    }) => {
      const { token, data } = args
      const payload = Token.verify(token)

      if (!payload) throw new Error('Unauthorized!')

      try {
        const user = await User.findByPk(payload.sub)
        if (!user) throw new Error("User doesn't exists!")

        Object.assign(user, data)
        await user.save()
        await user.reload({ include: 'posts' })

        return user.toJSON()
      } catch (error) {
        throw new Error(error.message || 'Internal server error!')
      }
    },
    login: async (args: { email: string; password: string }) => {
      const user = await User.findOne({
        where: { email: args.email },
        include: 'posts'
      })

      if (!user || !(await matchPassword(user, args.password))) {
        throw new Error("Wrong password or email doesn't exists!")
      }

      return {
        token: Token.sign({ sub: user.id, role: user.role }),
        data: user.toJSON()
      }
    }
  },
  post: {
    list: async () => {
      const posts = (await Post.findAll({ include: 'user' }))
        .map(post => post.toJSON() as any)
        .map(({ user: creator, ...post }) => ({ ...post, creator }))

      return posts
    },
    create: async (args: {
      data: {
        creator: string
        title: string
        content?: string
        imageURL?: string
      }
    }) => {
      const errors = [
        validator.isEmpty(args.data.title) && 'Please enter a title!',
        args.data.imageURL !== undefined &&
          (!args.data.imageURL.length ||
            !validator.isURL(args.data.imageURL)) &&
          'URL malformed!'
      ]
        .filter((msg): msg is string => !!msg)
        .map(msg => new Error(msg))

      if (errors.length) {
        throw errors[0]
      }

      const user = await User.findByPk(args.data.creator)
      if (!user) throw new Error('User not found!')

      const post = await user.createPost(args.data as any)
      await user.reload({ include: 'posts' })

      return { ...post.toJSON(), creator: user.toJSON() }
    }
  }
}
