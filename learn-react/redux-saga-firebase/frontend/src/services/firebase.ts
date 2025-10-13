// Firebase App (the core Firebase SDK) is always required and
// must be listed before other Firebase SDKs
import firebase from 'firebase/app'

// Add the Firebase services that you want to use
import 'firebase/auth'
import 'firebase/firestore'

// Your web app's Firebase configuration
import firebaseConfig from '.credentials/firebase-client.json'

// Initialize Firebase
firebase.initializeApp(firebaseConfig)

export default firebase
