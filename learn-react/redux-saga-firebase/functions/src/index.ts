import * as firebase from 'firebase-admin'
import * as functions from 'firebase-functions'

firebase.initializeApp()

function createNotification(noti: NotifyObject) {
  return firebase
    .firestore()
    .collection('notifications')
    .add(noti)
}

export const onProjectCreate = functions.firestore
  .document('projects/{projectID}')
  .onCreate(async snapshot => {
    const project = snapshot.data()

    if (!project) return

    await createNotification({
      detail: 'Added a new project',
      time: project.createdAt,
      who: {
        id: project.authorId,
        name: `${project.authorFirstName} ${project.authorLastName}`
      }
    })

    console.log(`"${project.title}" was created by ${project.authorId}`)
  })

export const onUserSignup = functions.auth.user().onCreate(async newUser => {
  const snapshot = await firebase
    .firestore()
    .collection('users')
    .doc(newUser.uid)
    .get()
  const data = snapshot.data()!

  await createNotification({
    detail: 'Joined the party',
    time: snapshot.createTime!,
    who: {
      id: newUser.uid,
      name: `${data.firstname} ${data.lastname}`
    }
  })

  console.log(`user "${newUser.uid}" signed up`)
})
